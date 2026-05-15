{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Lookup.SymbolsMap
  ( HomeSymbolsIndexCache (..),
    ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    SymbolsDependencySetCache (..),
    emptyHomeSymbolsIndexCache,
    emptyExternalSymbolsIndexCache,
    emptySymbolsDependencySetCache,
    getCachedSymbolsMap,
    getCachedHomeSymbolsIndex,
    getCachedExternalSymbolsIndex,
    prepareHomeSymbolsIndex,
    prepareExternalSymbolsIndex,
    setSymbolsDependencySetCache,
    readSymbolsDependencySetCache,
    invalidateHomeSymbolsIndexCache,
    invalidateExternalSymbolsIndexCache,
    findMatchingSymbolsInMap,
    findSimilarSymbolsInMap,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (forM, when)
import Control.Monad.Reader (MonadIO (..), asks)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Driver.Main as GHC
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Avail as GHC
import qualified GHC.Types.FieldLabel as GHC.FieldLabel
import qualified GHC.Types.TyThing as TyThing
import Lore.Internal.Definition.Cache.TypedModuleFacts (lookupTypedModuleFactsCache)
import Lore.Internal.Definition.Types (MinimalTypedModuleFacts (typedDefinitionNames, typedDefinitionOccAliases))
import Lore.Internal.Lookup.Cache.Types
  ( ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    HomeSymbolsIndexCache (..),
    SymbolsDependencySetCache (..),
  )
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Name (NormalizedName (..), NormalizedOccName, extractAndNormalizeModuleName, extractAndNormalizeOccName, normalizeName, parseAndNormalizeName, unNormalizedOccName)
import Lore.Internal.Lookup.Search.Score (buildSearchIndex, searchOccurrences)
import Lore.Internal.Lookup.Search.Types (SearchResult (..))
import Lore.Internal.Lookup.Types
  ( ModSummaries (..),
    Symbol (..),
    SymbolSuggestionCandidate (..),
    SymbolVisibility (..),
    SymbolsIndex (..),
    SymbolsMap (..),
    symbolExportedFrom,
  )
import Lore.Internal.Session (SessionContext (..))
import Lore.Lib.Force (evaluateNFM)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (SomeException, handle, modifyMVar, pooledForConcurrently, readMVar)

emptyHomeSymbolsIndexCache :: HomeSymbolsIndexCache
emptyHomeSymbolsIndexCache =
  HomeSymbolsIndexCache Nothing

emptyExternalSymbolsIndexCache :: ExternalSymbolsIndexCache
emptyExternalSymbolsIndexCache =
  ExternalSymbolsIndexCache Nothing

emptySymbolsDependencySetCache :: SymbolsDependencySetCache
emptySymbolsDependencySetCache =
  SymbolsDependencySetCache Set.empty

getCachedSymbolsMap :: (MonadLore m) => m SymbolsMap
getCachedSymbolsMap = do
  homeSymbolsMap <- getCachedHomeSymbolsIndex
  externalSymbolsMap <- getCachedExternalSymbolsIndex
  pure SymbolsMap {homeSymbolsMap, externalSymbolsMap}

findMatchingSymbolsInMap :: NormalizedName -> SymbolsMap -> Set.Set Symbol
findMatchingSymbolsInMap targetName SymbolsMap {homeSymbolsMap, externalSymbolsMap} =
  moduleMatchingSymbols
  where
    lookupSymbolsInIndex (SymbolsIndex symbolsMap) =
      Map.findWithDefault Set.empty targetName.occName symbolsMap
    homeMatchingSymbols = lookupSymbolsInIndex homeSymbolsMap
    externalMatchingSymbols = lookupSymbolsInIndex externalSymbolsMap
    moduleMatchingSymbols =
      Set.filter (isModuleMatching targetName) (homeMatchingSymbols <> externalMatchingSymbols)

findSimilarSymbolsInMap :: Int -> NormalizedName -> SymbolsMap -> [SymbolSuggestionCandidate]
findSimilarSymbolsInMap suggestionLimit targetName symbolsMap =
  map mkSymbolSuggestionCandidate $
    searchOccurrences suggestionLimit (unNormalizedOccName targetName.occName) searchIndex
  where
    SymbolsIndex combinedSymbolsIndex = combineSymbolsIndexes symbolsMap
    searchIndex =
      buildSearchIndex
        [ (occName, unNormalizedOccName occName, moduleMatchingSymbols)
        | (occName, symbols) <- Map.toList combinedSymbolsIndex,
          let moduleMatchingSymbols = Set.filter (isModuleMatching targetName) symbols,
          not (Set.null moduleMatchingSymbols)
        ]

    mkSymbolSuggestionCandidate result =
      SymbolSuggestionCandidate
        { suggestionCandidateSymbols = result.searchResultValue,
          suggestionCandidateLookupName = result.searchResultText,
          suggestionCandidateScore = result.searchResultScore
        }

combineSymbolsIndexes :: SymbolsMap -> SymbolsIndex
combineSymbolsIndexes SymbolsMap {homeSymbolsMap, externalSymbolsMap} =
  SymbolsIndex (Map.unionWith Set.union homeSymbols externalSymbols)
  where
    SymbolsIndex homeSymbols = homeSymbolsMap
    SymbolsIndex externalSymbols = externalSymbolsMap

isModuleMatching :: NormalizedName -> Symbol -> Bool
isModuleMatching targetName symbol =
  case targetName.moduleName of
    Nothing ->
      True
    Just hintedModule ->
      hintedModule `Set.member` symbolAssociatedModules
  where
    symbolName = normalizeName symbol.name
    definingModuleName = maybe Set.empty Set.singleton symbolName.moduleName
    exportingModuleNames = Set.map extractAndNormalizeModuleName (symbolExportedFrom symbol)
    symbolAssociatedModules = definingModuleName <> exportingModuleNames

invalidateHomeSymbolsIndexCache :: (MonadLore m) => m ()
invalidateHomeSymbolsIndexCache = do
  cacheVar <- asks homeSymbolsIndexCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptyHomeSymbolsIndexCache, ())

invalidateExternalSymbolsIndexCache :: (MonadLore m) => m ()
invalidateExternalSymbolsIndexCache = do
  cacheVar <- asks externalSymbolsIndexCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptyExternalSymbolsIndexCache, ())

setSymbolsDependencySetCache :: (MonadLore m) => Set.Set String -> m ()
setSymbolsDependencySetCache dependencies = do
  dependencyVar <- asks symbolsDependencySetCacheVar
  dependenciesChanged <- modifyMVar dependencyVar $ \(SymbolsDependencySetCache cachedDependencies) ->
    pure (SymbolsDependencySetCache dependencies, cachedDependencies /= dependencies)
  when dependenciesChanged do
    Log.debug $ "External symbol cache dependencies changed to " <> show (Set.toList dependencies) <> ". Invalidating external symbols cache."
    invalidateExternalSymbolsIndexCache

readSymbolsDependencySetCache :: (MonadLore m) => m (Set.Set String)
readSymbolsDependencySetCache = do
  dependencyVar <- asks symbolsDependencySetCacheVar
  SymbolsDependencySetCache dependencies <- liftIO (readMVar dependencyVar)
  pure dependencies

getCachedHomeSymbolsIndex :: (MonadLore m) => m SymbolsIndex
getCachedHomeSymbolsIndex = do
  cacheVar <- asks homeSymbolsIndexCacheVar
  modifyMVar cacheVar $ \cacheState ->
    case cacheState.cachedHomeSymbolsIndex of
      Just symbolsMap -> pure (cacheState, symbolsMap)
      Nothing -> do
        symbolsMap <- prepareHomeSymbolsIndex
        pure (HomeSymbolsIndexCache (Just symbolsMap), symbolsMap)

getCachedExternalSymbolsIndex :: (MonadLore m) => m SymbolsIndex
getCachedExternalSymbolsIndex = do
  currentDependencies <- readSymbolsDependencySetCache
  cacheVar <- asks externalSymbolsIndexCacheVar
  modifyMVar cacheVar $ \cacheState ->
    case cacheState.cachedExternalSymbolsSnapshot of
      Just cachedSnapshot
        | cachedSnapshot.externalSymbolsSnapshotDependencies == currentDependencies ->
            pure (cacheState, cachedSnapshot.externalSymbolsSnapshotIndex)
      _ -> do
        symbolsMap <- prepareExternalSymbolsIndex currentDependencies
        let snapshot =
              ExternalSymbolsSnapshot
                { externalSymbolsSnapshotDependencies = currentDependencies,
                  externalSymbolsSnapshotIndex = symbolsMap
                }
        pure (ExternalSymbolsIndexCache (Just snapshot), symbolsMap)

prepareHomeSymbolsIndex :: (MonadLore m) => m SymbolsIndex
prepareHomeSymbolsIndex = do
  Log.debug "Preparing symbols map for home modules..."
  homeModules <- enumerateHomeModules
  Log.debug $ "Enumerated " <> show (length homeModules) <> " home modules."
  homeModulesSymbols <- forM homeModules getHomeModuleSymbols
  Log.debug $ "Fetched symbols for " <> show (length homeModulesSymbols) <> " home modules."
  let symbolsMap = buildSymbolsIndex homeModulesSymbols
  logPreparedSymbolsIndex "home modules" symbolsMap
  pure symbolsMap

prepareExternalSymbolsIndex :: (MonadLore m) => Set.Set String -> m SymbolsIndex
prepareExternalSymbolsIndex dependencies = do
  Log.debug $ "Preparing symbols map for external modules with dependencies " <> show (Set.toList dependencies) <> "."
  externalModules <- enumerateVisiblePackageModules
  Log.debug $ "Enumerated " <> show (length externalModules) <> " visible package modules."
  hscEnv <- GHC.getSession
  externalModulesSymbols <- liftIO $ pooledForConcurrently externalModules (evaluateNFM . getExternalModuleSymbols hscEnv)
  Log.debug $ "Fetched symbols for " <> show (length externalModulesSymbols) <> " external modules."
  let symbolsMap = buildSymbolsIndex externalModulesSymbols
  logPreparedSymbolsIndex "external modules" symbolsMap
  pure symbolsMap

enumerateHomeModules :: (MonadLore m) => m [GHC.Module]
enumerateHomeModules = do
  ModSummaries summaries <- getCachedModSummaries
  pure $ map GHC.ms_mod (Map.elems summaries)

enumerateVisiblePackageModules :: (MonadLore m) => m [GHC.Module]
enumerateVisiblePackageModules = do
  hscEnv <- GHC.getSession
  let ust = GHC.hsc_units hscEnv
      visibleNames = GHC.listVisibleModuleNames ust
      mods =
        [ m
        | mn <- visibleNames,
          (m, _unitInfo) <- GHC.lookupModuleInAllUnits ust mn
        ]
  pure mods

data ModuleSymbolsResult
  = ModuleSymbolsLoaded GHC.Module [IndexableSymbol]
  | ModuleSymbolsMissing GHC.Module
  | ModuleSymbolsFailed GHC.Module
  deriving (Generic, NFData)

data IndexableSymbol = IndexableSymbol
  { indexableSymbol :: !Symbol,
    indexableRawOccurrenceKey :: !(Maybe NormalizedOccName),
    indexableAliasKeys :: !(Set.Set NormalizedOccName)
  }
  deriving (Generic, NFData)

getExternalModuleSymbols :: GHC.HscEnv -> GHC.Module -> IO ModuleSymbolsResult
getExternalModuleSymbols hsc_env mdl = do
  handle
    do \(_ :: SomeException) -> pure (ModuleSymbolsFailed mdl)
    do
      iface <- GHC.hscGetModuleInterface hsc_env mdl
      let exportedOccAliases = availInfosFieldAliases (GHC.mi_exports iface)
      exportedSymbols <-
        buildIndexableSymbols
          exportedOccAliases
          (GHC.lookupType hsc_env)
          (Symbol'ExportedFrom (Set.singleton mdl))
          (availInfosNameSet (GHC.mi_exports iface))
      pure (ModuleSymbolsLoaded mdl exportedSymbols)

getHomeModuleSymbols :: (MonadLore m) => GHC.Module -> m ModuleSymbolsResult
getHomeModuleSymbols mdl = do
  handle
    do \(_ :: SomeException) -> pure (ModuleSymbolsFailed mdl)
    do
      maybeModuleInfo <- GHC.getModuleInfo mdl
      case maybeModuleInfo of
        Nothing ->
          pure (ModuleSymbolsMissing mdl)
        Just _moduleInfo -> do
          maybeTypedModuleFacts <- lookupTypedModuleFactsCache mdl
          case maybeTypedModuleFacts of
            Nothing -> do
              Log.err $ "Missing typed module facts for loaded home module " <> moduleDisplayName mdl <> ". Failing home symbols indexing for this module."
              pure (ModuleSymbolsFailed mdl)
            Just typedModuleFacts -> do
              maybeExportedSymbols <- getExportedHomeModuleSymbols mdl
              case maybeExportedSymbols of
                Nothing -> do
                  Log.err $ "Unable to read module interface exports for loaded home module " <> moduleDisplayName mdl <> ". Failing home symbols indexing for this module."
                  pure (ModuleSymbolsFailed mdl)
                Just (exportedNameSet, exportedOccAliases) -> do
                  let definedNameSet =
                        getDefinedHomeModuleNames mdl typedModuleFacts
                      definedOccAliases =
                        getDefinedHomeModuleOccAliases mdl typedModuleFacts
                      occAliasesByName =
                        Map.unionWith Set.union definedOccAliases exportedOccAliases
                      unexportedNameSet =
                        Set.difference definedNameSet exportedNameSet
                  exportedSymbols <-
                    buildIndexableSymbols
                      occAliasesByName
                      GHC.lookupName
                      (Symbol'ExportedFrom (Set.singleton mdl))
                      exportedNameSet
                  unexportedSymbols <-
                    buildIndexableSymbols
                      occAliasesByName
                      GHC.lookupName
                      Symbol'Unexported
                      unexportedNameSet
                  pure (ModuleSymbolsLoaded mdl (exportedSymbols <> unexportedSymbols))

buildIndexableSymbols ::
  (Monad m) =>
  Map.Map GHC.Name (Set.Set NormalizedOccName) ->
  (GHC.Name -> m (Maybe TyThing.TyThing)) ->
  SymbolVisibility ->
  Set.Set GHC.Name ->
  m [IndexableSymbol]
buildIndexableSymbols occAliasesByName lookupTyThing visibility names =
  catMaybes <$> forM (Set.toList names) mkIndexableSymbol
  where
    mkIndexableSymbol name = do
      maybeTyThing <- lookupTyThing name
      let aliasKeys =
            Map.findWithDefault Set.empty name occAliasesByName
          rawOccurrenceKey =
            if
              | not (Set.null aliasKeys) -> Nothing
              | isUserFacingRawName name maybeTyThing -> Just (extractAndNormalizeOccName name)
              | otherwise -> Nothing
      pure
        if Set.null aliasKeys && rawOccurrenceKey == Nothing
          then Nothing
          else
            Just
              IndexableSymbol
                { indexableSymbol =
                    Symbol
                      { name,
                        visibility,
                        aliases = aliasKeys
                      },
                  indexableRawOccurrenceKey = rawOccurrenceKey,
                  indexableAliasKeys = aliasKeys
                }

isUserFacingTyThing :: TyThing.TyThing -> Bool
isUserFacingTyThing = \case
  TyThing.ACoAxiom {} -> False
  _ -> True

isUserFacingRawName :: GHC.Name -> Maybe TyThing.TyThing -> Bool
isUserFacingRawName name maybeTyThing =
  not (GHC.isSystemName name)
    && not (GHC.isDerivedOccName (GHC.nameOccName name))
    && maybe True isUserFacingTyThing maybeTyThing

getExportedHomeModuleSymbols :: (MonadLore m) => GHC.Module -> m (Maybe (Set.Set GHC.Name, Map.Map GHC.Name (Set.Set NormalizedOccName)))
getExportedHomeModuleSymbols homeModule = do
  hscEnv <- GHC.getSession
  maybeIface <-
    liftIO $
      handle
        do \(_ :: SomeException) -> pure Nothing
        do Just <$> GHC.hscGetModuleInterface hscEnv homeModule
  pure do
    iface <- maybeIface
    pure
      ( availInfosNameSet (GHC.mi_exports iface),
        availInfosFieldAliases (GHC.mi_exports iface)
      )

getDefinedHomeModuleNames :: GHC.Module -> MinimalTypedModuleFacts -> Set.Set GHC.Name
getDefinedHomeModuleNames homeModule typedModuleFacts =
  Set.fromList
    (filter isDefinedInCurrentModule (typedDefinitionNames typedModuleFacts))
  where
    isDefinedInCurrentModule name =
      case GHC.nameModule_maybe name of
        Just module_ -> module_ == homeModule
        Nothing -> False

getDefinedHomeModuleOccAliases :: GHC.Module -> MinimalTypedModuleFacts -> Map.Map GHC.Name (Set.Set NormalizedOccName)
getDefinedHomeModuleOccAliases homeModule typedModuleFacts =
  typedAliasesByName
  where
    typedAliasesByName =
      Map.fromListWith
        Set.union
        [ (name, normalizeOccAliases aliases)
        | (name, aliases) <- Map.toList (typedDefinitionOccAliases typedModuleFacts),
          GHC.nameModule_maybe name == Just homeModule
        ]

    normalizeOccAliases aliases =
      Set.map (\aliasText -> (parseAndNormalizeName aliasText).occName) aliases

availInfosNameSet :: [GHC.AvailInfo] -> Set.Set GHC.Name
availInfosNameSet availInfos =
  Set.fromList
    [ name
    | availInfo <- availInfos,
      name <- availInfoNamesWithFields availInfo
    ]

availInfoNamesWithFields :: GHC.AvailInfo -> [GHC.Name]
availInfoNamesWithFields = \case
  GHC.Avail greName ->
    [GHC.greNamePrintableName greName]
  GHC.AvailTC parentName subordinateNames ->
    parentName : map GHC.greNamePrintableName subordinateNames

availInfosFieldAliases :: [GHC.AvailInfo] -> Map.Map GHC.Name (Set.Set NormalizedOccName)
availInfosFieldAliases availInfos =
  Map.fromListWith
    Set.union
    [ (name, Set.singleton (normalizeOccAlias aliasText))
    | availInfo <- availInfos,
      greName <- availInfoGreNames availInfo,
      name <- [GHC.greNamePrintableName greName],
      Just aliasText <- [greNameFieldAliasText greName]
    ]
  where
    normalizeOccAlias aliasText =
      (parseAndNormalizeName aliasText).occName

availInfoGreNames :: GHC.AvailInfo -> [GHC.GreName]
availInfoGreNames = \case
  GHC.Avail greName ->
    [greName]
  GHC.AvailTC parentName subordinateNames ->
    GHC.NormalGreName parentName : subordinateNames

greNameFieldAliasText :: GHC.GreName -> Maybe T.Text
greNameFieldAliasText = \case
  GHC.FieldGreName fieldLabel ->
    Just (fieldLabelAliasText fieldLabel)
  GHC.NormalGreName _ ->
    Nothing

fieldLabelAliasText :: GHC.FieldLabel -> T.Text
fieldLabelAliasText fieldLabel =
  T.pack (GHC.getOccString (GHC.FieldLabel.fieldLabelPrintableName fieldLabel))

buildSymbolsIndex :: [ModuleSymbolsResult] -> SymbolsIndex
buildSymbolsIndex moduleSymbols =
  SymbolsIndex (mapToSymbols <$> foldl' insertModuleSymbols Map.empty moduleSymbols)
  where
    insertModuleSymbols acc = \case
      ModuleSymbolsLoaded _ symbols ->
        foldl' insertSymbol acc symbols
      ModuleSymbolsMissing _ ->
        acc
      ModuleSymbolsFailed _ ->
        acc

    insertSymbol acc indexableSymbol =
      foldl'
        ( \mapAcc key ->
            Map.insertWith
              (Map.unionWith mergeSymbolMeta)
              key
              (Map.singleton symbol.name (symbolMeta symbol))
              mapAcc
        )
        acc
        (Set.toList (symbolLookupKeys indexableSymbol))
      where
        symbol = indexableSymbol.indexableSymbol

    mergeSymbolMeta (newModules, newAliases) (oldModules, oldAliases) =
      (oldModules <> newModules, oldAliases <> newAliases)

    symbolMeta symbol =
      (exportedFromSet symbol.visibility, symbol.aliases)

    symbolLookupKeys indexableSymbol =
      maybe
        indexableSymbol.indexableAliasKeys
        (`Set.insert` indexableSymbol.indexableAliasKeys)
        indexableSymbol.indexableRawOccurrenceKey

    exportedFromSet = \case
      Symbol'ExportedFrom modules_ -> modules_
      Symbol'Unexported -> Set.empty

    mapToSymbols =
      Set.fromList . map mkSymbol . Map.toList

    mkSymbol (symbolName, (modules, aliases)) =
      let visibility = if Set.null modules then Symbol'Unexported else Symbol'ExportedFrom modules
       in Symbol symbolName visibility aliases

logPreparedSymbolsIndex :: (MonadLore m) => String -> SymbolsIndex -> m ()
logPreparedSymbolsIndex scope (SymbolsIndex symbolsMap) = do
  let symbolsCount = sum (map length (Map.elems symbolsMap))
  Log.debug $ "Collected " <> show symbolsCount <> " symbols from " <> scope <> "."
  Log.debug $ "Prepared symbols map with " <> show (Map.size symbolsMap) <> " unique symbol names for " <> scope <> "."

moduleDisplayName :: GHC.Module -> String
moduleDisplayName =
  GHC.moduleNameString . GHC.moduleName
