{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Lookup
  ( NormalizedOccName,
    NormalizedModuleName,
    NormalizedName (occName, moduleName, ownerHint),
    parseAndNormalizeName,
    normalizeModuleName,
    mkNormalizedModuleName,
    Symbol (..),
    SymbolSuggestion (..),
    SymbolVisibility (..),
    SymbolCategory (..),
    SymbolInfo (..),
    Instances (..),
    ChosenInstanceError (..),
    ChosenInstanceResolution (..),
    ChosenInstanceContextStatus (..),
    PathToRoot (..),
    ModulePattern,
    ModulePatternError (..),
    compileModulePattern,
    FindSimilarSymbolsOptions (..),
    classifySymbolCategory,
    findMatchingSymbols,
    findMatchingSymbolLookupNamesByPrefix,
    findProjectModuleNamesByPrefix,
    findSimilarSymbols,
    lookupSymbolInfo,
    listIntersectingInstances,
    listAssociatedInstances,
    listDirectInstances,
    resolveChosenClassInstanceFromTypeText,
    resolvePathToRoot,
  )
where

import Control.Monad (filterM)
import Data.List (foldl', sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Core.FamInstEnv as FamInstEnv
import qualified GHC.Core.InstEnv as InstEnv
import qualified GHC.Core.RoughMap as RoughMap
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as GHC
import Lore.Internal.Lookup.InstanceResolution
  ( ChosenInstanceContextStatus (..),
    ChosenInstanceError (..),
    ChosenInstanceResolution (..),
    resolveChosenClassInstanceFromTypeText,
  )
import Lore.Internal.Lookup.ModulePattern (ModulePattern, ModulePatternError (..), compileModulePattern)
import Lore.Internal.Lookup.Name (NormalizedModuleName, NormalizedName (..), NormalizedOccName (..), mkNormalizedModuleName, normalizeModuleName, normalizeName, parseAndNormalizeName)
import Lore.Internal.Lookup.NameToInstances (getCachedNameToInstancesIndex)
import Lore.Internal.Lookup.SymbolsMap (findMatchingSymbolsInMap, findSimilarSymbolsInMap)
import qualified Lore.Internal.Lookup.SymbolsMap as SymbolsMap
import Lore.Internal.Lookup.Types (NameToInstancesIndex (..), Symbol (..), SymbolSuggestion (..), SymbolVisibility (..), SymbolsIndex (..), SymbolsMap (..))
import qualified Lore.Internal.Package as Package
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

data FindSimilarSymbolsOptions = FindSimilarSymbolsOptions
  { similarSymbolsLimit :: Int,
    similarSymbolsModulePatterns :: [ModulePattern]
  }
  deriving stock (Eq, Show)

findMatchingSymbols :: (MonadLore m) => NormalizedName -> m (Set.Set Symbol)
findMatchingSymbols targetName = do
  symbolsMap <- SymbolsMap.getCachedSymbolsMap
  filterSymbolsByOwnerHint targetName.ownerHint (findMatchingSymbolsInMap targetName symbolsMap)

findMatchingSymbolLookupNamesByPrefix :: (MonadLore m) => T.Text -> m [T.Text]
findMatchingSymbolLookupNamesByPrefix rawPrefix = do
  symbolsMap <- SymbolsMap.getCachedSymbolsMap
  pure (matchingSymbolLookupNamesByPrefix rawPrefix symbolsMap)

findProjectModuleNamesByPrefix :: (MonadLore m) => T.Text -> m [T.Text]
findProjectModuleNamesByPrefix rawPrefix = do
  packages <- Package.discoverProject
  let allProjectModuleNames =
        Set.fromList
          [ T.pack (GHC.moduleNameString ghcModuleName)
          | packageData <- packages,
            componentData <- packageData.components,
            ghcModuleName <- Set.toList componentData.modules
          ]
  pure
    [ moduleName
    | moduleName <- sortOn id (Set.toList allProjectModuleNames),
      rawPrefix `T.isPrefixOf` moduleName
    ]

matchingSymbolLookupNamesByPrefix :: T.Text -> SymbolsMap -> [T.Text]
matchingSymbolLookupNamesByPrefix rawPrefix SymbolsMap {homeSymbolsMap, externalSymbolsMap} =
  map unNormalizedOccName (sortOn unNormalizedOccName (Set.toList matchingNames))
  where
    normalizedPrefix = (parseAndNormalizeName rawPrefix).occName
    matchingNames =
      Set.filter
        (isLookupNameMatchingPrefix normalizedPrefix)
        (Map.keysSet homeSymbolsMap.symbolsByLookupName <> Map.keysSet externalSymbolsMap.symbolsByLookupName)

isLookupNameMatchingPrefix :: NormalizedOccName -> NormalizedOccName -> Bool
isLookupNameMatchingPrefix prefix lookupName =
  unNormalizedOccName prefix `T.isPrefixOf` unNormalizedOccName lookupName

findSimilarSymbols :: (MonadLore m) => FindSimilarSymbolsOptions -> NormalizedName -> m [SymbolSuggestion]
findSimilarSymbols options targetName = do
  symbolsMap <- SymbolsMap.getCachedSymbolsMap
  suggestions <- findSimilarSymbolsInMap options.similarSymbolsModulePatterns targetName symbolsMap
  pure $
    take options.similarSymbolsLimit $
      sortOn suggestionSortKey $
        Map.elems $
          foldl' collectBestSuggestion Map.empty suggestions
  where
    suggestionSortKey suggestion =
      ( Down suggestion.suggestionScore,
        suggestion.suggestedLookupName,
        suggestion.suggestedSymbol.name
      )

    collectBestSuggestion suggestionsByName suggestion =
      Map.insertWith
        pickBetterSuggestion
        suggestion.suggestedSymbol.name
        suggestion
        suggestionsByName

    pickBetterSuggestion newSuggestion oldSuggestion
      | newSuggestion.suggestionScore > oldSuggestion.suggestionScore = newSuggestion
      | newSuggestion.suggestionScore < oldSuggestion.suggestionScore = oldSuggestion
      | otherwise = oldSuggestion

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
  maybe Symbol'Unexported (.visibility) (lookupSymbolInMap symbolsMap name)

lookupSymbolInMap :: SymbolsMap -> GHC.Name -> Maybe Symbol
lookupSymbolInMap symbolsMap name =
  case Set.toList matchingSymbols of
    [] -> Nothing
    symbol : _ -> Just symbol
  where
    normalizedName = normalizeName name
    matchingSymbols = Set.filter (\symbol -> symbol.name == name) $ findMatchingSymbolsInMap normalizedName symbolsMap

filterSymbolsByOwnerHint :: (MonadLore m) => Maybe NormalizedOccName -> Set.Set Symbol -> m (Set.Set Symbol)
filterSymbolsByOwnerHint maybeOwnerHint symbols =
  case maybeOwnerHint of
    Nothing ->
      pure symbols
    Just ownerHint' ->
      Set.fromList <$> filterM (symbolMatchesOwnerHint ownerHint') (Set.toList symbols)

symbolMatchesOwnerHint :: (MonadLore m) => NormalizedOccName -> Symbol -> m Bool
symbolMatchesOwnerHint ownerHint' symbol = do
  pathToRoot <- resolvePathToRoot symbol.name
  pure $ any (matchesOwnerHint ownerHint') (drop 1 (NE.toList pathToRoot.unPathToRoot))
  where
    matchesOwnerHint hintedOwner name =
      (parseAndNormalizeName (T.pack (GHC.getOccString name))).occName == hintedOwner

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
