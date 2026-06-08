{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Lookup.SymbolsMap
  ( HomeSymbolsIndexCache (..),
    ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    SimilarSymbolsSearchIndexCache (..),
    SymbolsDependencySetCache (..),
    emptyHomeSymbolsIndexCache,
    emptyExternalSymbolsIndexCache,
    emptySimilarSymbolsSearchIndexCache,
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
    findSimilarSymbolsCandidatesInMap,
    buildSimilarSymbolsSearchIndex,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (forM, when)
import Control.Monad.Reader (MonadIO (..), asks)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Driver.Main as GHC
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Avail as GHC
import qualified GHC.Types.Name as GHC.Name
import qualified GHC.Types.SrcLoc as GHC.SrcLoc
import Lore.Internal.Definition.Cache.TypedModuleFacts (lookupTypedModuleFactsCache)
import Lore.Internal.Definition.Types (MinimalTypedModuleFacts (..), TypedNameFacts (..), typedInstanceNames)
import Lore.Internal.Ghc.AvailInfo (availInfoGreNames, availInfosNameSet, greNameFieldAliasText)
import Lore.Internal.Lookup.Cache.Types
  ( ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    HomeSymbolsIndexCache (..),
    SimilarSymbolSearchKey (..),
    SimilarSymbolsSearchIndex (..),
    SimilarSymbolsSearchIndexCache (..),
    SymbolsDependencySetCache (..),
  )
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Name (NormalizedModuleName, NormalizedName (..), NormalizedOccName, extractAndNormalizeModuleName, extractAndNormalizeOccName, normalizeName, parseAndNormalizeName, unNormalizedModuleName, unNormalizedOccName)
import Lore.Internal.Lookup.Search.Score (buildSearchIndex, searchOccurrences)
import Lore.Internal.Lookup.Search.Types (SearchDocument (..), SearchResult (..), TokenSearchIndex)
import Lore.Internal.Lookup.Types
  ( ModSummaries (..),
    Symbol (..),
    SymbolSuggestion (..),
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

emptySimilarSymbolsSearchIndexCache :: SimilarSymbolsSearchIndexCache
emptySimilarSymbolsSearchIndexCache =
  SimilarSymbolsSearchIndexCache Nothing

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

findSimilarSymbolsInMap :: (MonadLore m) => NormalizedName -> SymbolsMap -> m [SymbolSuggestion]
findSimilarSymbolsInMap targetName symbolsMap = do
  SimilarSymbolsSearchIndex cachedSearchIndex <- getCachedSimilarSymbolsSearchIndex symbolsMap
  pure $ findSimilarSymbolsCandidatesInMap targetName cachedSearchIndex

findSimilarSymbolsCandidatesInMap ::
  NormalizedName ->
  TokenSearchIndex SimilarSymbolSearchKey Symbol ->
  [SymbolSuggestion]
findSimilarSymbolsCandidatesInMap targetName searchIndex =
  mapMaybe mkSymbolSuggestion $
    searchOccurrences (unNormalizedOccName targetName.occName) searchIndex
  where
    mkSymbolSuggestion result
      | isModuleMatching targetName result.searchResultValue =
          Just
            SymbolSuggestion
              { suggestedSymbol = result.searchResultValue,
                suggestedLookupName = result.searchResultText,
                suggestionScore = result.searchResultScore
              }
      | otherwise =
          Nothing

combineSymbolsIndexes :: SymbolsMap -> SymbolsIndex
combineSymbolsIndexes SymbolsMap {homeSymbolsMap, externalSymbolsMap} =
  SymbolsIndex (Map.unionWith Set.union homeSymbols externalSymbols)
  where
    SymbolsIndex homeSymbols = homeSymbolsMap
    SymbolsIndex externalSymbols = externalSymbolsMap

buildSimilarSymbolsSearchIndex :: SymbolsMap -> TokenSearchIndex SimilarSymbolSearchKey Symbol
buildSimilarSymbolsSearchIndex symbolsMap =
  buildSearchIndex
    [ ( SimilarSymbolSearchKey {searchLookupName = occName, searchSymbolName = symbol.name},
        SearchDocument
          { primaryText = unNormalizedOccName occName,
            secondaryTexts = maybe [] ((: []) . unNormalizedModuleName) (symbolDefiningModuleName symbol)
          },
        symbol
      )
    | (occName, symbols) <- Map.toList combinedSymbolsIndex,
      symbol <- Set.toList symbols
    ]
  where
    SymbolsIndex combinedSymbolsIndex = combineSymbolsIndexes symbolsMap

isModuleMatching :: NormalizedName -> Symbol -> Bool
isModuleMatching targetName symbol =
  case targetName.moduleName of
    Nothing ->
      True
    Just hintedModule ->
      hintedModule `Set.member` symbolAssociatedModules
  where
    symbolAssociatedModules = symbolAssociatedModuleNames symbol

symbolAssociatedModuleNames :: Symbol -> Set.Set NormalizedModuleName
symbolAssociatedModuleNames symbol =
  maybe Set.empty Set.singleton (symbolDefiningModuleName symbol)
    <> Set.map extractAndNormalizeModuleName (symbolExportedFrom symbol)

symbolDefiningModuleName :: Symbol -> Maybe NormalizedModuleName
symbolDefiningModuleName symbol =
  (normalizeName symbol.name).moduleName

invalidateHomeSymbolsIndexCache :: (MonadLore m) => m ()
invalidateHomeSymbolsIndexCache = do
  cacheVar <- asks homeSymbolsIndexCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptyHomeSymbolsIndexCache, ())
  invalidateSimilarSymbolsSearchIndexCache

invalidateExternalSymbolsIndexCache :: (MonadLore m) => m ()
invalidateExternalSymbolsIndexCache = do
  cacheVar <- asks externalSymbolsIndexCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptyExternalSymbolsIndexCache, ())
  invalidateSimilarSymbolsSearchIndexCache

invalidateSimilarSymbolsSearchIndexCache :: (MonadLore m) => m ()
invalidateSimilarSymbolsSearchIndexCache = do
  cacheVar <- asks similarSymbolsSearchIndexCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptySimilarSymbolsSearchIndexCache, ())

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

getCachedSimilarSymbolsSearchIndex :: (MonadLore m) => SymbolsMap -> m SimilarSymbolsSearchIndex
getCachedSimilarSymbolsSearchIndex symbolsMap = do
  cacheVar <- asks similarSymbolsSearchIndexCacheVar
  modifyMVar cacheVar $ \cacheState ->
    case cacheState.cachedSimilarSymbolsSearchIndex of
      Just cachedSearchIndex ->
        pure (cacheState, cachedSearchIndex)
      Nothing -> do
        let builtSearchIndex = buildSimilarSymbolsSearchIndex symbolsMap
            snapshot = SimilarSymbolsSearchIndex builtSearchIndex
        pure (SimilarSymbolsSearchIndexCache (Just snapshot), snapshot)

prepareHomeSymbolsIndex :: (MonadLore m) => m SymbolsIndex
prepareHomeSymbolsIndex = do
  Log.debug "Preparing symbols map for home modules..."
  homeModules <- enumerateHomeModules
  Log.debug $ "Enumerated " <> show (length homeModules) <> " home modules."
  let homeModulesSet = Set.fromList homeModules
  homeModulesSymbols <- forM homeModules (getHomeModuleSymbols homeModulesSet)
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
          exportedSymbols =
            buildIndexableSymbolsDetailed
              exportedOccAliases
              isExternalRawOccurrenceName
              (Symbol'ExportedFrom (Set.singleton mdl))
              (availInfosNameSet (GHC.mi_exports iface))
      pure (ModuleSymbolsLoaded mdl exportedSymbols)

getHomeModuleSymbols :: (MonadLore m) => Set.Set GHC.Module -> GHC.Module -> m ModuleSymbolsResult
getHomeModuleSymbols homeModules mdl = do
  handle
    do \(_ :: SomeException) -> pure (ModuleSymbolsFailed mdl)
    do
      maybeModuleInfo <- GHC.getModuleInfo mdl
      case maybeModuleInfo of
        Nothing ->
          pure (ModuleSymbolsMissing mdl)
        Just moduleInfo -> do
          maybeTypedModuleFacts <- lookupTypedModuleFactsCache mdl
          case maybeTypedModuleFacts of
            Nothing -> do
              Log.err $ "Missing typed module facts for loaded home module " <> moduleDisplayName mdl <> ". Failing home symbols indexing for this module."
              pure (ModuleSymbolsFailed mdl)
            Just typedModuleFacts -> do
              let exportedNameSet =
                    getExportedHomeModuleNames mdl homeModules moduleInfo typedModuleFacts
                  exportedOccAliases =
                    getExportedHomeModuleOccAliases mdl homeModules moduleInfo typedModuleFacts
                  definedNameSet =
                    getDefinedHomeModuleNames mdl typedModuleFacts
                      `Set.difference` typedInstanceNames typedModuleFacts
                  definedOccAliases =
                    getDefinedHomeModuleOccAliases mdl typedModuleFacts
                  occAliasesByName =
                    Map.unionWith Set.union definedOccAliases exportedOccAliases
                  unexportedNameSet =
                    Set.difference definedNameSet exportedNameSet
                  exportedSymbols =
                    buildIndexableSymbolsDetailed
                      occAliasesByName
                      isHomeRawOccurrenceName
                      (Symbol'ExportedFrom (Set.singleton mdl))
                      exportedNameSet
                  unexportedSymbols =
                    buildIndexableSymbolsDetailed
                      occAliasesByName
                      isHomeRawOccurrenceName
                      Symbol'Unexported
                      unexportedNameSet
              pure (ModuleSymbolsLoaded mdl (exportedSymbols <> unexportedSymbols))

buildIndexableSymbolsDetailed ::
  Map.Map GHC.Name (Set.Set NormalizedOccName) ->
  (GHC.Name -> Bool) ->
  SymbolVisibility ->
  Set.Set GHC.Name ->
  [IndexableSymbol]
buildIndexableSymbolsDetailed occAliasesByName isRawOccurrenceName visibility names =
  mapMaybe mkIndexableSymbol (Set.toList names)
  where
    mkIndexableSymbol name =
      let isUserFacingName =
            isRawOccurrenceName name
          aliasKeys =
            Map.findWithDefault Set.empty name occAliasesByName
          rawOccurrenceKey =
            if
              | not isUserFacingName -> Nothing
              | not (Set.null aliasKeys) -> Nothing
              | otherwise -> Just (extractAndNormalizeOccName name)
       in if not isUserFacingName || (Set.null aliasKeys && rawOccurrenceKey == Nothing)
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

isExternalRawOccurrenceName :: GHC.Name -> Bool
isExternalRawOccurrenceName name =
  not (GHC.isSystemName name)
    && not (GHC.isDerivedOccName (GHC.nameOccName name))

isHomeRawOccurrenceName :: GHC.Name -> Bool
isHomeRawOccurrenceName name =
  not (GHC.isSystemName name)
    && not (GHC.Name.isInternalName name)
    && not (GHC.isDerivedOccName (GHC.nameOccName name))
    && not (GHC.SrcLoc.isGeneratedSrcSpan (GHC.Name.nameSrcSpan name))

getExportedHomeModuleNames :: GHC.Module -> Set.Set GHC.Module -> GHC.ModuleInfo -> MinimalTypedModuleFacts -> Set.Set GHC.Name
getExportedHomeModuleNames homeModule homeModules moduleInfo typedModuleFacts =
  moduleDefinedExports <> reexportedHomeModuleExports
  where
    nameFacts =
      typedModuleFacts.typedNameFacts

    moduleDefinedExports =
      Set.fromList nameFacts.typedExportedNames

    reexportedHomeModuleExports =
      Set.fromList (reexportedHomeModuleNames homeModule homeModules moduleInfo)

getExportedHomeModuleOccAliases :: GHC.Module -> Set.Set GHC.Module -> GHC.ModuleInfo -> MinimalTypedModuleFacts -> Map.Map GHC.Name (Set.Set NormalizedOccName)
getExportedHomeModuleOccAliases homeModule homeModules moduleInfo typedModuleFacts =
  Map.fromListWith
    Set.union
    (moduleDefinedAliases <> reexportedHomeModuleAliases)
  where
    nameFacts =
      typedModuleFacts.typedNameFacts

    moduleDefinedAliases =
      [ (name, normalizeOccAliases aliases)
      | (name, aliases) <- Map.toList nameFacts.typedExportedOccAliases
      ]

    reexportedHomeModuleAliases =
      [ (name, Set.singleton (extractAndNormalizeOccName name))
      | name <- reexportedHomeModuleNames homeModule homeModules moduleInfo
      ]

    normalizeOccAliases aliases =
      Set.map (\aliasText -> (parseAndNormalizeName aliasText).occName) aliases

reexportedHomeModuleNames :: GHC.Module -> Set.Set GHC.Module -> GHC.ModuleInfo -> [GHC.Name]
reexportedHomeModuleNames homeModule homeModules moduleInfo =
  filter isReexportedHomeModuleName (GHC.modInfoExports moduleInfo)
  where
    isReexportedHomeModuleName name =
      GHC.nameModule_maybe name /= Just homeModule
        && maybe False (`Set.member` homeModules) (GHC.nameModule_maybe name)

getDefinedHomeModuleNames :: GHC.Module -> MinimalTypedModuleFacts -> Set.Set GHC.Name
getDefinedHomeModuleNames homeModule typedModuleFacts =
  Set.fromList
    (filter isDefinedInCurrentModule typedModuleFacts.typedNameFacts.typedDefinitionNames)
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
        | (name, aliases) <- Map.toList typedModuleFacts.typedNameFacts.typedDefinitionOccAliases,
          GHC.nameModule_maybe name == Just homeModule
        ]

    normalizeOccAliases aliases =
      Set.map (\aliasText -> (parseAndNormalizeName aliasText).occName) aliases

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
