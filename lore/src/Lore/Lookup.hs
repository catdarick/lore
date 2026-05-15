{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Lookup
  ( NormalizedOccName,
    NormalizedModuleName,
    NormalizedName (occName, moduleName),
    parseAndNormalizeName,
    normalizeModuleName,
    mkNormalizedModuleName,
    Symbol (..),
    SymbolVisibility (..),
    SymbolCategory (..),
    SymbolInfo (..),
    Instances (..),
    PathToRoot (..),
    classifySymbolCategory,
    findMatchingSymbols,
    findMatchingSymbolsRoots,
    lookupSymbolInfo,
    listIntersectingInstances,
    listAssociatedInstances,
    listDirectInstances,
    resolvePathToRoot,
    mergePathsToRootOn,
  )
where

import Control.Monad (forM)
import Data.List (foldl', isInfixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Core.FamInstEnv as FamInstEnv
import qualified GHC.Core.InstEnv as InstEnv
import qualified GHC.Core.RoughMap as RoughMap
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as GHC
import Lore.Internal.Lookup.Name (NormalizedModuleName, NormalizedName (..), NormalizedOccName, mkNormalizedModuleName, normalizeModuleName, normalizeName, parseAndNormalizeName)
import Lore.Internal.Lookup.NameToInstances (getCachedNameToInstancesIndex)
import Lore.Internal.Lookup.SymbolsMap (findMatchingSymbolsInMap)
import qualified Lore.Internal.Lookup.SymbolsMap as SymbolsMap
import Lore.Internal.Lookup.Types (NameToInstancesIndex (..), Symbol (..), SymbolVisibility (..), SymbolsMap)
import Lore.Monad (MonadLore)

data SymbolInfo = SymbolInfo
  { symbolName :: GHC.Name,
    definedIn :: GHC.Module,
    visibility :: SymbolVisibility,
    symbolThing :: GHC.TyThing,
    symbolType :: Maybe GHC.Type
  }

data SymbolCategory
  = SymbolValue
  | SymbolData
  | SymbolNewtype
  | SymbolTypeAlias
  | SymbolClass
  | SymbolTypeFamily
  | SymbolDataFamily
  | SymbolConstructor
  | SymbolCoercionAxiom
  | SymbolUnknown
  deriving stock (Eq, Show)

findMatchingSymbols :: (MonadLore m) => NormalizedName -> m (Set.Set Symbol)
findMatchingSymbols targetName = do
  findMatchingSymbolsInMap targetName <$> SymbolsMap.getCachedSymbolsMap

findMatchingSymbolsRoots :: (MonadLore m) => NormalizedName -> m (Set.Set Symbol)
findMatchingSymbolsRoots targetName = do
  symbolsMap <- SymbolsMap.getCachedSymbolsMap
  let matchingSymbols = findMatchingSymbolsInMap targetName symbolsMap
  pathsToRoot <- forM (Set.toList matchingSymbols) $ \symbol -> do
    resolvePathToRoot symbol.name
  dedupedRootNames <- dedupeRootNamesByNormalizedOcc (map getPathRoot pathsToRoot)
  pure $ Set.fromList $ map (mkSymbolFromName symbolsMap) dedupedRootNames

dedupeRootNamesByNormalizedOcc :: (MonadLore m) => [GHC.Name] -> m [GHC.Name]
dedupeRootNamesByNormalizedOcc names =
  concat <$> mapM pickPreferredName (Map.elems namesByNormalized)
  where
    namesByNormalized =
      Map.fromListWith
        (<>)
        [ (normalizeName name, [name])
        | name <- names
        ]

    pickPreferredName [] =
      pure []
    pickPreferredName namesForNormalizedOcc = do
      categorizedNames <-
        forM namesForNormalizedOcc $ \name -> do
          maybeTyThing <- GHC.lookupName name
          pure (name, maybe SymbolUnknown classifySymbolCategory maybeTyThing)
      let nonValueNames =
            [ name
            | (name, category) <- categorizedNames,
              category /= SymbolValue
            ]
      pure
        case nonValueNames of
          [] -> take 1 namesForNormalizedOcc
          (preferredName : _) -> [preferredName]

lookupSymbolInfo :: (MonadLore m) => GHC.Name -> m (Maybe SymbolInfo)
lookupSymbolInfo name = do
  symbolsMap <- SymbolsMap.getCachedSymbolsMap
  case GHC.nameModule_maybe name of
    Nothing -> do
      pure Nothing
    Just m -> do
      maybeTyThing <- GHC.lookupName name
      case maybeTyThing of
        Nothing -> do
          pure Nothing
        Just tyThing -> do
          let symbolType = case tyThing of
                GHC.AnId id' -> Just (GHC.idType id')
                _ -> Nothing
          pure $
            Just
              SymbolInfo
                { symbolName = name,
                  definedIn = m,
                  visibility = getNameVisibility symbolsMap name,
                  symbolThing = tyThing,
                  symbolType = symbolType
                }

getNameVisibility :: SymbolsMap -> GHC.Name -> SymbolVisibility
getNameVisibility symbolsMap name = do
  let normalizedName = normalizeName name
      matchingSymbols = Set.filter (\symbol -> symbol.name == name) $ findMatchingSymbolsInMap normalizedName symbolsMap
  case Set.toList matchingSymbols of
    [] -> Symbol'Unexported
    (symbol : _) -> symbol.visibility

mkSymbolFromName :: SymbolsMap -> GHC.Name -> Symbol
mkSymbolFromName symbolsMap name =
  Symbol
    { name,
      visibility = getNameVisibility symbolsMap name
    }

classifySymbolCategory :: GHC.TyThing -> SymbolCategory
classifySymbolCategory = \case
  GHC.AnId {} -> SymbolValue
  GHC.AConLike {} -> SymbolConstructor
  GHC.ACoAxiom {} -> SymbolCoercionAxiom
  GHC.ATyCon tyCon
    | GHC.isClassTyCon tyCon -> SymbolClass
    | GHC.isDataFamilyTyCon tyCon -> SymbolDataFamily
    | GHC.isTypeFamilyTyCon tyCon -> SymbolTypeFamily
    | GHC.isTypeSynonymTyCon tyCon -> SymbolTypeAlias
    | GHC.isNewTyCon tyCon -> SymbolNewtype
    | GHC.isDataTyCon tyCon -> SymbolData
    | otherwise -> SymbolUnknown

data Instances = Instances
  { classInstances :: [InstEnv.ClsInst],
    familyInstances :: [FamInstEnv.FamInst]
  }

listIntersectingInstances :: (MonadLore m) => [GHC.Name] -> m Instances
listIntersectingInstances targetNames = do
  instancesPerName <- mapM listAssociatedInstances targetNames
  case instancesPerName of
    [] -> pure (Instances [] [])
    (firstInstances : restInstances) ->
      pure $ foldl' intersectMatchingInstances firstInstances restInstances

listAssociatedInstances :: (MonadLore m) => GHC.Name -> m Instances
listAssociatedInstances name = do
  NameToInstancesIndex nameToInstancesIndex <- getCachedNameToInstancesIndex
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> pure (Instances [] [])
    Just (clsInsts, famInsts) ->
      pure $
        Instances
          { classInstances = dedupeInstancesByName clsInsts,
            familyInstances = dedupeInstancesByName famInsts
          }

listDirectInstances :: (MonadLore m) => GHC.Name -> m Instances
listDirectInstances name = do
  associatedInstances <- listAssociatedInstances name
  maybeTyThing <- GHC.lookupName name
  pure $
    case maybeTyThing of
      Nothing -> Instances [] []
      Just tyThing -> filterDirectInstances name tyThing associatedInstances

filterDirectInstances :: GHC.Name -> GHC.TyThing -> Instances -> Instances
filterDirectInstances targetName targetTyThing associatedInstances =
  case targetTyThing of
    GHC.ATyCon tyCon
      | GHC.isClassTyCon tyCon ->
          Instances
            { classInstances = filter (isDirectClassInstance targetName) associatedInstances.classInstances,
              familyInstances = []
            }
      | GHC.isTypeFamilyTyCon tyCon || GHC.isDataFamilyTyCon tyCon ->
          Instances
            { classInstances = filter (mentionsTyConDirectlyInClassHead targetName) associatedInstances.classInstances,
              familyInstances = filter (isDirectFamilyInstance targetName) associatedInstances.familyInstances
            }
      | otherwise ->
          Instances
            { classInstances = filter (mentionsTyConDirectlyInClassHead targetName) associatedInstances.classInstances,
              familyInstances = filter (mentionsTyConDirectlyInFamilyHead targetName) associatedInstances.familyInstances
            }
    _ ->
      Instances [] []

isDirectClassInstance :: GHC.Name -> InstEnv.ClsInst -> Bool
isDirectClassInstance targetClassName classInstance =
  GHC.getName classInstance.is_cls == targetClassName

isDirectFamilyInstance :: GHC.Name -> FamInstEnv.FamInst -> Bool
isDirectFamilyInstance targetFamilyName familyInstance =
  familyInstance.fi_fam == targetFamilyName

mentionsTyConDirectlyInClassHead :: GHC.Name -> InstEnv.ClsInst -> Bool
mentionsTyConDirectlyInClassHead targetTyConName classInstance =
  any (isDirectRoughMatchTc targetTyConName) classInstance.is_tcs

mentionsTyConDirectlyInFamilyHead :: GHC.Name -> FamInstEnv.FamInst -> Bool
mentionsTyConDirectlyInFamilyHead targetTyConName familyInstance =
  any (isDirectRoughMatchTc targetTyConName) familyInstance.fi_tcs

isDirectRoughMatchTc :: GHC.Name -> RoughMap.RoughMatchTc -> Bool
isDirectRoughMatchTc targetTyConName roughMatchTc =
  case roughMatchTc of
    RoughMap.RM_KnownTc roughTyConName ->
      roughTyConName == targetTyConName
    RoughMap.RM_WildCard ->
      False

dedupeInstancesByName :: (GHC.NamedThing a) => [a] -> [a]
dedupeInstancesByName =
  reverse . snd . foldl' step (Set.empty, [])
  where
    step (seenNames, acc) instance_ =
      let instanceName = GHC.getName instance_
       in if Set.member instanceName seenNames
            then (seenNames, acc)
            else (Set.insert instanceName seenNames, instance_ : acc)

intersectMatchingInstances :: Instances -> Instances -> Instances
intersectMatchingInstances left right =
  Instances
    { classInstances = filter (\i -> GHC.getName i `Set.member` rightClassIndex) (classInstances left),
      familyInstances = filter (\i -> GHC.getName i `Set.member` rightFamilyIndex) (familyInstances left)
    }
  where
    rightClassIndex = Set.fromList (map GHC.getName (classInstances right))
    rightFamilyIndex = Set.fromList (map GHC.getName (familyInstances right))

newtype PathToRoot a = PathToRoot
  { unPathToRoot :: NE.NonEmpty a -- Root is last element, queried symbol is first element
  }
  deriving newtype (Functor)

getPathRoot :: PathToRoot a -> a
getPathRoot = NE.last . unPathToRoot

resolvePathToRoot :: (MonadLore m) => GHC.Name -> m (PathToRoot GHC.Name)
resolvePathToRoot name = do
  maybeTyThing <- GHC.lookupName name
  pure $
    case maybeTyThing of
      Nothing ->
        PathToRoot (NE.singleton name)
      Just tyThing ->
        PathToRoot (NE.fromList (map GHC.getName (collectRootTyThingChain tyThing)))
  where
    collectRootTyThingChain tyThing =
      tyThing : maybe [] collectRootTyThingChain (GHC.tyThingParent_maybe tyThing)

mergePathsToRootOn :: (Ord b) => (a -> b) -> [PathToRoot a] -> [PathToRoot a]
mergePathsToRootOn getKey pathes =
  let foo = [(getKey (getPathRoot path), path) | path <- pathes]
      merged = Map.fromListWith mergePaths foo
   in Map.elems merged
  where
    mergePaths nePath1 nePath2 =
      let path1 = NE.toList (unPathToRoot nePath1)
          path2 = NE.toList (unPathToRoot nePath2)
          path1Keys = map getKey path1
          path2Keys = map getKey path2
       in if
            | path1Keys `isInfixOf` path2Keys -> nePath2
            | path2Keys `isInfixOf` path1Keys -> nePath1
            | otherwise ->
                let (primaryPath, secondaryPath) = if length path1 >= length path2 then (path1, path2) else (path2, path1)
                    primaryPathKeys = map getKey primaryPath
                    secondaryUniquePart = filter (\a -> getKey a `notElem` primaryPathKeys) secondaryPath
                 in PathToRoot $ NE.fromList (secondaryUniquePart <> primaryPath)
