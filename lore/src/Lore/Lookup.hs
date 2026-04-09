{-# OPTIONS_GHC -Wno-orphans #-}

module Lore.Lookup
  ( ExportedSymbol (..),
    SymbolCategory (..),
    SymbolInfo (..),
    Instances (..),
    LookupInstancesQuery (..),
    MatchingInstance (..),
    LookupInstancesResult (..),
    findSymbols,
    lookupSymbolInfo,
    lookupRootSymbolInfo,
    lookupIntersectingInstances,
    lookupIntersectingRootInstances,
    resolveInstances,
    resolveInstanceDefinitions,
  )
where

import Control.Monad (forM)
import Data.Char (isAlphaNum, isUpper)
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (foldl', intercalate, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Core.FamInstEnv as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as GHC
import Lore.Definition (DefinitionSlice, resolveDefinitionSlice)
import Lore.Internal.Lookup.NameToInstances (getNameToInstancesIndex)
import qualified Lore.Internal.Lookup.SymbolsMap as SymbolsMap
import Lore.Internal.Lookup.Types (ExportedSymbol (..), NameToInstancesIndex (..), SymbolsMap)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

findSymbols :: (MonadLore m) => Text -> m [ExportedSymbol]
findSymbols needle = do
  symbolsMap <- SymbolsMap.getSymbolsMap
  pure (findExportedSymbols needle symbolsMap)

data SymbolInfo = SymbolInfo
  { symbolName :: GHC.Name,
    definedIn :: GHC.Module,
    exportedFrom :: [GHC.Module],
    symbolThing :: Maybe GHC.TyThing,
    symbolCategory :: SymbolCategory,
    symbolType :: Maybe GHC.Type,
    associatedClassInstances :: [GHC.ClsInst],
    associatedFamilyInstances :: [GHC.FamInst]
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

instance Show SymbolInfo where
  show si =
    "Symbol: "
      <> showName si.symbolName
      <> ",\n Defined in: "
      <> showModule si.definedIn
      <> ",\n Exported from: "
      <> showModules si.exportedFrom
      <> ",\n Class instances: "
      <> intercalate ", " (map showClassInst si.associatedClassInstances)
      <> ",\n Family instances: "
      <> intercalate ", " (map showFamInst si.associatedFamilyInstances)
    where
      showName n = case GHC.nameModule_maybe n of
        Nothing -> "<UNKNOWN>." <> GHC.occNameString (GHC.nameOccName n)
        Just m -> GHC.moduleNameString (GHC.moduleName m) <> "." <> GHC.occNameString (GHC.nameOccName n)
      showModule m = GHC.moduleNameString (GHC.moduleName m)
      showModules xs = intercalate ", " (map showModule xs)
      showClassInst clsInst = GHC.showSDocUnsafe $ GHC.pprInstance clsInst
      showFamInst famInst =
        let famName = GHC.getOccString (GHC.fi_fam famInst)
         in famName

lookupSymbolInfo :: (MonadLore m) => Text -> m [SymbolInfo]
lookupSymbolInfo = getSymbolInfo' False

lookupRootSymbolInfo :: (MonadLore m) => Text -> m [SymbolInfo]
lookupRootSymbolInfo = getSymbolInfo' True

getSymbolInfo' :: (MonadLore m) => Bool -> Text -> m [SymbolInfo]
getSymbolInfo' resolveRoot needle = do
  symbolsMap <- SymbolsMap.getSymbolsMap
  let names = findExportedSymbols needle symbolsMap
  resolvedSymbols <-
    if resolveRoot
      then resolveRootExportedSymbols symbolsMap names
      else pure names
  Log.debug $ "Found " <> show (length resolvedSymbols) <> " symbols matching query \"" <> T.unpack needle <> "\"."
  catMaybes <$> forM resolvedSymbols getExportedSymbolInfo

getExportedSymbolInfo :: (MonadLore m) => ExportedSymbol -> m (Maybe SymbolInfo)
getExportedSymbolInfo es = do
  case GHC.nameModule_maybe es.name of
    Nothing -> do
      Log.warn $ "Symbol " <> GHC.showSDocUnsafe (GHC.ppr es.name) <> " does not have an associated module. Skipping instance resolution."
      pure Nothing
    Just m -> do
      Log.debug $ "Looking up symbol: " <> GHC.showSDocUnsafe (GHC.ppr es.name)
      tyThing <- GHC.lookupName es.name
      let symbolCategory = classifySymbolCategory tyThing
          symbolType = case tyThing of
            Nothing -> Nothing
            Just tt -> case tt of
              GHC.AnId id' -> Just (GHC.idType id')
              _ -> Nothing
      Log.debug $ "Symbol " <> GHC.showSDocUnsafe (GHC.ppr es.name) <> " is categorized as " <> show symbolCategory <> "."
      instancesInfo <- resolveInstances es.name
      Log.debug $ "Symbol " <> GHC.showSDocUnsafe (GHC.ppr es.name) <> " has " <> show (maybe 0 (length . classInstances) instancesInfo) <> " class instances and " <> show (maybe 0 (length . familyInstances) instancesInfo) <> " family instances."
      pure $
        Just
          SymbolInfo
            { symbolName = es.name,
              definedIn = m,
              exportedFrom = es.exportedFrom,
              symbolThing = tyThing,
              symbolCategory = symbolCategory,
              symbolType = symbolType,
              associatedClassInstances = maybe [] classInstances instancesInfo,
              associatedFamilyInstances = maybe [] familyInstances instancesInfo
            }

data Instances = Instances
  { classInstances :: [GHC.ClsInst],
    familyInstances :: [GHC.FamInst]
  }

classifySymbolCategory :: Maybe GHC.TyThing -> SymbolCategory
classifySymbolCategory = \case
  Nothing -> SymbolUnknown
  Just tyThing ->
    case tyThing of
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

resolveRootExportedSymbols :: (MonadLore m) => SymbolsMap -> [ExportedSymbol] -> m [ExportedSymbol]
resolveRootExportedSymbols symbolsMap exportedSymbols = do
  Log.debug $ "Resolving root symbols for " <> show (length exportedSymbols) <> " exported symbols."
  r <- deduplicateExportedSymbols <$> mapM (resolveRootExportedSymbol symbolsMap) exportedSymbols
  Log.debug "Finished resolving root symbols."
  pure r

resolveRootExportedSymbol :: (MonadLore m) => SymbolsMap -> ExportedSymbol -> m ExportedSymbol
resolveRootExportedSymbol symbolsMap exportedSymbol = do
  rootName <- resolveRootName exportedSymbol.name
  pure $
    case SymbolsMap.lookupExportedSymbolByNameInMap rootName symbolsMap of
      Just rootExportedSymbol -> rootExportedSymbol
      Nothing -> exportedSymbol

deduplicateExportedSymbols :: [ExportedSymbol] -> [ExportedSymbol]
deduplicateExportedSymbols =
  sortOn (renderOutputable . exportedSymbolName)
    . nubOrdOn exportedSymbolName

exportedSymbolName :: ExportedSymbol -> GHC.Name
exportedSymbolName exportedSymbol =
  exportedSymbol.name

data LookupInstancesQuery = LookupInstancesQuery
  { lookupInstancesQueryText :: Text,
    lookupInstancesQueryMatches :: [GHC.Name]
  }

data MatchingInstance
  = MatchingClassInstance GHC.Name GHC.ClsInst
  | MatchingFamilyInstance GHC.Name GHC.FamInst

data LookupInstancesResult = LookupInstancesResult
  { lookupInstancesQueries :: [LookupInstancesQuery],
    lookupInstancesResults :: [MatchingInstance]
  }

lookupIntersectingInstances :: (MonadLore m) => [Text] -> m LookupInstancesResult
lookupIntersectingInstances =
  lookupIntersectingInstances' False

lookupIntersectingRootInstances :: (MonadLore m) => [Text] -> m LookupInstancesResult
lookupIntersectingRootInstances =
  lookupIntersectingInstances' True

lookupIntersectingInstances' :: (MonadLore m) => Bool -> [Text] -> m LookupInstancesResult
lookupIntersectingInstances' resolveRoot queries = do
  resolvedQueries <- mapM (resolveLookupInstancesQuery resolveRoot) queries
  pure $
    LookupInstancesResult
      { lookupInstancesQueries = map fst resolvedQueries,
        lookupInstancesResults = intersectAllMatchingInstances (map snd resolvedQueries)
      }

resolveLookupInstancesQuery :: (MonadLore m) => Bool -> Text -> m (LookupInstancesQuery, [MatchingInstance])
resolveLookupInstancesQuery resolveRoot queryText = do
  matchedNames <- findResolvedSymbolNames resolveRoot queryText
  matchedInstances <- lookupMatchingInstancesForNames matchedNames
  pure
    ( LookupInstancesQuery
        { lookupInstancesQueryText = queryText,
          lookupInstancesQueryMatches = matchedNames
        },
      matchedInstances
    )

findResolvedSymbolNames :: (MonadLore m) => Bool -> Text -> m [GHC.Name]
findResolvedSymbolNames resolveRoot needle = do
  symbolsMap <- SymbolsMap.getSymbolsMap
  let exportedSymbols = findExportedSymbols needle symbolsMap
  deduplicateNames <$> mapM (resolveExportedSymbolName resolveRoot) exportedSymbols

findExportedSymbols :: Text -> SymbolsMap -> [ExportedSymbol]
findExportedSymbols queryText symbolsMap =
  filterExportedSymbolsByModuleHint moduleHint $
    SymbolsMap.lookupSymbolsInMap normalizedOccName symbolsMap
  where
    (moduleHint, occName) =
      splitQualifiedSymbolQuery queryText
    normalizedOccName =
      normalizeQueryOccName occName

    filterExportedSymbolsByModuleHint Nothing exportedSymbols =
      exportedSymbols
    filterExportedSymbolsByModuleHint (Just hintedModule) exportedSymbols =
      filter (exportedSymbolMatchesModuleHint hintedModule) exportedSymbols

    exportedSymbolMatchesModuleHint hintedModule exportedSymbol =
      symbolDefinedInModuleHint hintedModule exportedSymbol
        || any (moduleMatchesHint hintedModule) exportedSymbol.exportedFrom

    symbolDefinedInModuleHint hintedModule exportedSymbol =
      maybe False (moduleMatchesHint hintedModule) (GHC.nameModule_maybe exportedSymbol.name)

    moduleMatchesHint hintedModule module_ =
      T.pack (GHC.moduleNameString (GHC.moduleName module_)) == hintedModule

splitQualifiedSymbolQuery :: Text -> (Maybe Text, Text)
splitQualifiedSymbolQuery queryText =
  case qualifiedCandidates of
    (moduleHint, occName) : _ ->
      (Just moduleHint, occName)
    [] ->
      (Nothing, queryText)
  where
    segments = T.splitOn "." queryText
    qualifiedCandidates =
      reverse $
        mapMaybe mkCandidate [1 .. length segments - 1]

    mkCandidate prefixLen = do
      let moduleSegments = take prefixLen segments
          occSegments = drop prefixLen segments
          moduleHint = T.intercalate "." moduleSegments
          occName = T.intercalate "." occSegments
      if all isModuleNameSegment moduleSegments && not (T.null occName)
        then Just (moduleHint, occName)
        else Nothing

isModuleNameSegment :: Text -> Bool
isModuleNameSegment segment =
  case T.uncons segment of
    Nothing ->
      False
    Just (firstChar, rest) ->
      isUpper firstChar && T.all isModuleNameChar rest

isModuleNameChar :: Char -> Bool
isModuleNameChar char =
  isAlphaNum char || char == '_' || char == '\''

normalizeQueryOccName :: Text -> Text
normalizeQueryOccName occName =
  case T.stripPrefix "(" occName >>= T.stripSuffix ")" of
    Just strippedOccName
      | isOperatorOccName strippedOccName ->
          strippedOccName
    _ ->
      occName

isOperatorOccName :: Text -> Bool
isOperatorOccName text =
  not (T.null text) && T.all isOperatorChar text

isOperatorChar :: Char -> Bool
isOperatorChar char =
  char `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

resolveExportedSymbolName :: (MonadLore m) => Bool -> ExportedSymbol -> m GHC.Name
resolveExportedSymbolName resolveRoot exportedSymbol
  | resolveRoot =
      resolveRootName exportedSymbol.name
  | otherwise =
      pure exportedSymbol.name

lookupMatchingInstancesForNames :: (MonadLore m) => [GHC.Name] -> m [MatchingInstance]
lookupMatchingInstancesForNames names = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  pure $
    deduplicateMatchingInstances $
      concatMap (matchingInstancesForName nameToInstancesIndex) names

matchingInstancesForName :: GHC.NameEnv ([GHC.ClsInst], [GHC.FamInst]) -> GHC.Name -> [MatchingInstance]
matchingInstancesForName nameToInstancesIndex name =
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> []
    Just (clsInsts, famInsts) ->
      map (\clsInst -> MatchingClassInstance (GHC.getName clsInst) clsInst) clsInsts
        <> map (\famInst -> MatchingFamilyInstance (GHC.getName famInst) famInst) famInsts

intersectAllMatchingInstances :: [[MatchingInstance]] -> [MatchingInstance]
intersectAllMatchingInstances = \case
  [] -> []
  firstInstances : rest ->
    foldl' intersectMatchingInstances firstInstances rest

intersectMatchingInstances :: [MatchingInstance] -> [MatchingInstance] -> [MatchingInstance]
intersectMatchingInstances left right =
  filter (\matchingInstance -> GHC.elemNameEnv (matchingInstanceName matchingInstance) rightIndex) left
  where
    rightIndex =
      GHC.mkNameEnv [(matchingInstanceName matchingInstance, ()) | matchingInstance <- right]

deduplicateNames :: [GHC.Name] -> [GHC.Name]
deduplicateNames =
  sortOn renderOutputable
    . GHC.nonDetNameEnvElts
    . GHC.mkNameEnv
    . map (\name -> (name, name))

deduplicateMatchingInstances :: [MatchingInstance] -> [MatchingInstance]
deduplicateMatchingInstances =
  sortOn (renderOutputable . matchingInstanceName)
    . GHC.nonDetNameEnvElts
    . GHC.mkNameEnv
    . map (\matchingInstance -> (matchingInstanceName matchingInstance, matchingInstance))

matchingInstanceName :: MatchingInstance -> GHC.Name
matchingInstanceName = \case
  MatchingClassInstance name _ -> name
  MatchingFamilyInstance name _ -> name

resolveInstances :: (MonadLore m) => GHC.Name -> m (Maybe Instances)
resolveInstances name = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> pure Nothing
    Just (clsInsts, famInsts) -> pure $ Just (Instances clsInsts famInsts)

resolveInstanceDefinitions :: (MonadLore m) => GHC.Name -> m [DefinitionSlice]
resolveInstanceDefinitions name = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> pure []
    Just (clsInsts, famInsts) -> do
      let allNames = [GHC.getName clsInst | clsInst <- clsInsts] ++ [GHC.getName famInst | famInst <- famInsts]
      resolved <- mapM resolveDefinitionSlice allNames
      pure $ mapMaybe id resolved

resolveRootName :: (MonadLore m) => GHC.Name -> m GHC.Name
resolveRootName name = do
  mTyThing <- GHC.lookupName name
  pure $
    maybe name (GHC.getName . rootTyThing) mTyThing
  where
    rootTyThing :: GHC.TyThing -> GHC.TyThing
    rootTyThing tyThing =
      maybe tyThing rootTyThing (GHC.tyThingParent_maybe tyThing)

renderOutputable :: (GHC.Outputable a) => a -> String
renderOutputable =
  GHC.showSDocUnsafe . GHC.ppr
