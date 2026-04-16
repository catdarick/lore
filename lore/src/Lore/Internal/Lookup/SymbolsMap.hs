{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Lookup.SymbolsMap
  ( getSymbolsMap,
    invalidateHomeSymbolsMapCache,
    setSymbolsMapDependencies,
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
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Name (NormalizedName (..), extractAndNormalizeOccName)
import Lore.Internal.Lookup.Types (ExternalPackagesSymbolsCache (..), ModSummaries (..), Symbol (..), SymbolVisibility (..), SymbolsIndex (..), SymbolsMap (..), isSymbolNameMatching)
import Lore.Internal.Session (SessionContext (..))
import Lore.Lib.Force (evaluateNFM)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (SomeException, forConcurrently, handle, modifyMVar, readMVar)

getSymbolsMap :: (MonadLore m) => m SymbolsMap
getSymbolsMap = do
  homeSymbolsMap <- getHomeSymbolsMap
  externalSymbolsMap <- getExternalSymbolsMap
  pure SymbolsMap {homeSymbolsMap, externalSymbolsMap}

findMatchingSymbolsInMap :: NormalizedName -> SymbolsMap -> Set.Set Symbol
findMatchingSymbolsInMap targetName SymbolsMap {homeSymbolsMap, externalSymbolsMap} =
  Set.filter (isSymbolNameMatching targetName) (homeMatchingSymbols <> externalMatchingSymbols)
  where
    lookupSymbolsInIndex (SymbolsIndex symbolsMap) =
      Map.findWithDefault Set.empty targetName.occName symbolsMap
    homeMatchingSymbols = lookupSymbolsInIndex homeSymbolsMap
    externalMatchingSymbols = lookupSymbolsInIndex externalSymbolsMap

invalidateHomeSymbolsMapCache :: (MonadLore m) => m ()
invalidateHomeSymbolsMapCache = do
  cacheVar <- asks homeModulesSymbolsCache
  modifyMVar cacheVar $ \_ -> pure (Nothing, ())

setSymbolsMapDependencies :: (MonadLore m) => Set.Set String -> m ()
setSymbolsMapDependencies dependencies = do
  dependencyVar <- asks symbolsMapDependencySet
  dependenciesChanged <- modifyMVar dependencyVar $ \cachedDependencies ->
    pure (dependencies, cachedDependencies /= dependencies)
  when dependenciesChanged do
    Log.debug $ "External symbol cache dependencies changed to " <> show (Set.toList dependencies) <> ". Invalidating external symbols cache."
    cacheVar <- asks externalPackagesSymbolsCache
    modifyMVar cacheVar $ \_ -> pure (Nothing, ())

getHomeSymbolsMap :: (MonadLore m) => m SymbolsIndex
getHomeSymbolsMap = do
  cacheVar <- asks homeModulesSymbolsCache
  modifyMVar cacheVar $ \case
    Just symbolsMap -> pure (Just symbolsMap, symbolsMap)
    Nothing -> do
      symbolsMap <- prepareHomeSymbolsMap
      pure (Just symbolsMap, symbolsMap)

getExternalSymbolsMap :: (MonadLore m) => m SymbolsIndex
getExternalSymbolsMap = do
  dependencyVar <- asks symbolsMapDependencySet
  currentDependencies <- liftIO $ readMVar dependencyVar
  cacheVar <- asks externalPackagesSymbolsCache
  modifyMVar cacheVar $ \case
    Just cache
      | cache.externalPackagesDependencies == currentDependencies ->
          pure (Just cache, cache.externalPackagesSymbolsMap)
    _ -> do
      symbolsMap <- prepareExternalSymbolsMap currentDependencies
      let cache =
            ExternalPackagesSymbolsCache
              { externalPackagesDependencies = currentDependencies,
                externalPackagesSymbolsMap = symbolsMap
              }
      pure (Just cache, symbolsMap)

prepareHomeSymbolsMap :: (MonadLore m) => m SymbolsIndex
prepareHomeSymbolsMap = do
  Log.debug "Preparing symbols map for home modules..."
  homeModules <- enumerateHomeModules
  Log.debug $ "Enumerated " <> show (length homeModules) <> " home modules."
  homeModulesSymbols <- forM homeModules getHomeModuleSymbols
  Log.debug $ "Fetched symbols for " <> show (length homeModulesSymbols) <> " home modules."
  let symbolsMap = buildSymbolsIndex homeModulesSymbols
  logPreparedSymbolsIndex "home modules" symbolsMap
  pure symbolsMap

prepareExternalSymbolsMap :: (MonadLore m) => Set.Set String -> m SymbolsIndex
prepareExternalSymbolsMap dependencies = do
  Log.debug $ "Preparing symbols map for external modules with dependencies " <> show (Set.toList dependencies) <> "."
  externalModules <- enumerateVisiblePackageModules
  Log.debug $ "Enumerated " <> show (length externalModules) <> " visible package modules."
  hscEnv <- GHC.getSession
  externalModulesSymbols <- liftIO $ forConcurrently externalModules (evaluateNFM . getExternalModuleSymbols hscEnv)
  Log.debug $ "Fetched symbols for " <> show (length externalModulesSymbols) <> " external modules."
  let symbolsMap = buildSymbolsIndex externalModulesSymbols
  logPreparedSymbolsIndex "external modules" symbolsMap
  pure symbolsMap

enumerateHomeModules :: (MonadLore m) => m [GHC.Module]
enumerateHomeModules = do
  ModSummaries summaries <- getModSummaries
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
