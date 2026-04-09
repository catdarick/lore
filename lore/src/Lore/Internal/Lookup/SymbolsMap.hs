module Lore.Internal.Lookup.SymbolsMap
  ( getSymbolsMap,
    invalidateHomeSymbolsMapCache,
    setSymbolsMapDependencies,
    lookupSymbolsInMap,
    lookupExportedSymbolByNameInMap,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Reader (MonadIO (..), asks)
import Data.List (find, foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Driver.Main as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Avail as GHC
import qualified GHC.Unit.Env as GHC
import qualified GHC.Unit.Home.ModInfo as GHC
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Types
  ( ExportedSymbol (..),
    ExternalPackagesSymbolsCache (..),
    ModSummaries (..),
    SymbolsIndex (..),
    SymbolsMap (..),
  )
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (SomeException, handle, modifyMVar, readMVar)

getSymbolsMap :: (MonadLore m) => m SymbolsMap
getSymbolsMap = do
  homeSymbolsMap <- getHomeSymbolsMap
  externalSymbolsMap <- getExternalSymbolsMap
  pure SymbolsMap {homeSymbolsMap, externalSymbolsMap}

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

lookupSymbolsInMap :: Text -> SymbolsMap -> [ExportedSymbol]
lookupSymbolsInMap queryText SymbolsMap {homeSymbolsMap, externalSymbolsMap} =
  lookupSymbolsInIndex queryText homeSymbolsMap <> lookupSymbolsInIndex queryText externalSymbolsMap

lookupExportedSymbolByNameInMap :: GHC.Name -> SymbolsMap -> Maybe ExportedSymbol
lookupExportedSymbolByNameInMap name symbolsMap =
  find (\candidate -> candidate.name == name) (lookupSymbolsInMap occName symbolsMap)
  where
    occName = T.pack (GHC.getOccString name)

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
  hscEnv <- GHC.getSession
  homeModulesExports <- liftIO $ forM homeModules $ getHomeModuleExports hscEnv
  Log.debug $ "Fetched exports for " <> show (length homeModulesExports) <> " home modules."
  logModuleExportIssues homeModulesExports
  let symbolsMap = buildSymbolsIndex homeModulesExports
  logPreparedSymbolsIndex "home modules" symbolsMap
  pure symbolsMap

prepareExternalSymbolsMap :: (MonadLore m) => Set.Set String -> m SymbolsIndex
prepareExternalSymbolsMap dependencies = do
  Log.debug $ "Preparing symbols map for external modules with dependencies " <> show (Set.toList dependencies) <> "."
  externalModules <- enumerateVisiblePackageModules
  Log.debug $ "Enumerated " <> show (length externalModules) <> " visible package modules."
  hscEnv <- GHC.getSession
  externalModulesExports <- liftIO $ forM externalModules $ getExternalModuleExports hscEnv
  Log.debug $ "Fetched exports for " <> show (length externalModulesExports) <> " external modules."
  logModuleExportIssues externalModulesExports
  let symbolsMap = buildSymbolsIndex externalModulesExports
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

data ModuleExportsResult
  = ModuleExportsLoaded GHC.Module [GHC.Name]
  | ModuleExportsMissing GHC.Module
  | ModuleExportsFailed GHC.Module SomeException

getExternalModuleExports :: GHC.HscEnv -> GHC.Module -> IO ModuleExportsResult
getExternalModuleExports hsc_env mdl = do
  handle
    do \(e :: SomeException) -> pure (ModuleExportsFailed mdl e)
    do
      iface <- GHC.hscGetModuleInterface hsc_env mdl
      pure $ ModuleExportsLoaded mdl $ concatMap GHC.availNames $ GHC.mi_exports iface

getHomeModuleExports :: GHC.HscEnv -> GHC.Module -> IO ModuleExportsResult
getHomeModuleExports hsc_env mdl = do
  handle
    do \(e :: SomeException) -> pure (ModuleExportsFailed mdl e)
    do
      case GHC.lookupHugByModule mdl (GHC.hsc_HUG hsc_env) of
        Nothing -> pure (ModuleExportsMissing mdl)
        Just hmi -> pure $ ModuleExportsLoaded mdl $ concatMap GHC.availNames $ GHC.mi_exports $ GHC.hm_iface hmi

lookupSymbolsInIndex :: Text -> SymbolsIndex -> [ExportedSymbol]
lookupSymbolsInIndex queryText (SymbolsIndex symbolsMap) =
  Map.findWithDefault [] queryText symbolsMap

buildSymbolsIndex :: [ModuleExportsResult] -> SymbolsIndex
buildSymbolsIndex moduleExports =
  SymbolsIndex $
    fmap toExportedSymbols $
      foldl' insertModuleExports Map.empty moduleExports
  where
    insertModuleExports grouped = \case
      ModuleExportsLoaded exportedFrom names ->
        foldl' (insertExportedName exportedFrom) grouped names
      ModuleExportsMissing _ ->
        grouped
      ModuleExportsFailed _ _ ->
        grouped

    insertExportedName exportedFrom grouped exportedName =
      Map.insertWith
        (Map.unionWith (<>))
        (T.pack (GHC.getOccString exportedName))
        (Map.singleton exportedName [exportedFrom])
        grouped

    toExportedSymbols =
      map (\(exportedName, modules) -> ExportedSymbol exportedName modules)
        . Map.toList

logPreparedSymbolsIndex :: (MonadLore m) => String -> SymbolsIndex -> m ()
logPreparedSymbolsIndex scope (SymbolsIndex symbolsMap) = do
  let exportedSymbolsCount = sum (map length (Map.elems symbolsMap))
  Log.debug $ "Collected " <> show exportedSymbolsCount <> " exported symbols from " <> scope <> "."
  Log.debug $ "Prepared symbols map with " <> show (Map.size symbolsMap) <> " unique symbol names for " <> scope <> "."

logModuleExportIssues :: (MonadLore m) => [ModuleExportsResult] -> m ()
logModuleExportIssues =
  mapM_ \case
    ModuleExportsLoaded _ _ ->
      pure ()
    ModuleExportsMissing module_ ->
      Log.warn $ "Module info not found for " <> show module_.moduleName
    ModuleExportsFailed module_ err ->
      Log.err $ "Failed to get module info for " <> show module_.moduleName <> ": " <> show err
