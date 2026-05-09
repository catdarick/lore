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
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (forM, when)
import Control.Monad.Reader (MonadIO (..), asks)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Driver.Main as GHC
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Avail as GHC
import Lore.Internal.Lookup.Cache.Types
  ( ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    HomeSymbolsIndexCache (..),
    SymbolsDependencySetCache (..),
  )
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Name (NormalizedName (..), extractAndNormalizeOccName)
import Lore.Internal.Lookup.Types
  ( ModSummaries (..),
    Symbol (..),
    SymbolVisibility (..),
    SymbolsIndex (..),
    SymbolsMap (..),
    isSymbolNameMatching,
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
  Set.filter (isSymbolNameMatching targetName) (homeMatchingSymbols <> externalMatchingSymbols)
  where
    lookupSymbolsInIndex (SymbolsIndex symbolsMap) =
      Map.findWithDefault Set.empty targetName.occName symbolsMap
    homeMatchingSymbols = lookupSymbolsInIndex homeSymbolsMap
    externalMatchingSymbols = lookupSymbolsInIndex externalSymbolsMap

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
  = ModuleSymbolsLoaded GHC.Module (Set.Set Symbol)
  | ModuleSymbolsMissing GHC.Module
  | ModuleSymbolsFailed GHC.Module
  deriving (Generic, NFData)

getExternalModuleSymbols :: GHC.HscEnv -> GHC.Module -> IO ModuleSymbolsResult
getExternalModuleSymbols hsc_env mdl = do
  handle
    do \(_ :: SomeException) -> pure (ModuleSymbolsFailed mdl)
    do
      iface <- GHC.hscGetModuleInterface hsc_env mdl
      let exportedNames = Set.fromList $ concatMap GHC.availNames (GHC.mi_exports iface)
          exportedSymbols = flip Set.map exportedNames \exportedName ->
            Symbol
              { name = exportedName,
                visibility = Symbol'ExportedFrom (Set.singleton mdl)
              }
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
        Just moduleInfo -> do
          let exportedNameSet =
                Set.fromList (GHC.modInfoExports moduleInfo)
              topLevelNameSet =
                Set.fromList (fromMaybe [] (GHC.modInfoTopLevelScope moduleInfo))
              definedTopLevelNameSet =
                Set.filter isDefinedInCurrentModule topLevelNameSet
              unexportedNameSet =
                Set.difference definedTopLevelNameSet exportedNameSet
              exportedSymbols = flip Set.map exportedNameSet \exportedName ->
                Symbol
                  { name = exportedName,
                    visibility = Symbol'ExportedFrom (Set.singleton mdl)
                  }
              unexportedSymbols = flip Set.map unexportedNameSet \unexportedName ->
                Symbol
                  { name = unexportedName,
                    visibility = Symbol'Unexported
                  }
          pure (ModuleSymbolsLoaded mdl (exportedSymbols <> unexportedSymbols))
  where
    isDefinedInCurrentModule name =
      case GHC.nameModule_maybe name of
        Just module_ -> module_ == mdl
        Nothing -> False

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

    insertSymbol acc symbol =
      Map.insertWith
        (Map.unionWith Set.union)
        (extractAndNormalizeOccName symbol.name)
        (Map.singleton symbol.name (exportedFromSet symbol.visibility))
        acc

    exportedFromSet = \case
      Symbol'ExportedFrom modules_ -> modules_
      Symbol'Unexported -> Set.empty

    mapToSymbols =
      Set.fromList . map mkSymbol . Map.toList

    mkSymbol (symbolName, modules) =
      let visibility = if Set.null modules then Symbol'Unexported else Symbol'ExportedFrom modules
       in Symbol symbolName visibility

logPreparedSymbolsIndex :: (MonadLore m) => String -> SymbolsIndex -> m ()
logPreparedSymbolsIndex scope (SymbolsIndex symbolsMap) = do
  let symbolsCount = sum (map length (Map.elems symbolsMap))
  Log.debug $ "Collected " <> show symbolsCount <> " symbols from " <> scope <> "."
  Log.debug $ "Prepared symbols map with " <> show (Map.size symbolsMap) <> " unique symbol names for " <> scope <> "."
