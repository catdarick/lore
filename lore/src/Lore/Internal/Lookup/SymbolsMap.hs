module Lore.Internal.Lookup.SymbolsMap
  ( getSymbolsMap,
    invalidateHomeSymbolsMapCache,
    setSymbolsMapDependencies,
    lookupSymbolsInMap,
    lookupSymbolByNameInMap,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Reader (MonadIO (..), asks)
import Data.List (find, foldl', nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Driver.Main as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Avail as GHC
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Types (ExternalPackagesSymbolsCache (..), ModSummaries (..), Symbol (..), SymbolVisibility (..), SymbolsIndex (..), SymbolsMap (..))
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

lookupSymbolsInMap :: Text -> SymbolsMap -> [Symbol]
lookupSymbolsInMap queryText SymbolsMap {homeSymbolsMap, externalSymbolsMap} =
  lookupSymbolsInIndex queryText homeSymbolsMap <> lookupSymbolsInIndex queryText externalSymbolsMap

lookupSymbolByNameInMap :: GHC.Name -> SymbolsMap -> Maybe Symbol
lookupSymbolByNameInMap name symbolsMap =
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
  homeModulesSymbols <- forM homeModules getHomeModuleSymbols
  Log.debug $ "Fetched symbols for " <> show (length homeModulesSymbols) <> " home modules."
  logModuleSymbolIssues homeModulesSymbols
  let symbolsMap = buildSymbolsIndex homeModulesSymbols
  logPreparedSymbolsIndex "home modules" symbolsMap
  pure symbolsMap

prepareExternalSymbolsMap :: (MonadLore m) => Set.Set String -> m SymbolsIndex
prepareExternalSymbolsMap dependencies = do
  Log.debug $ "Preparing symbols map for external modules with dependencies " <> show (Set.toList dependencies) <> "."
  externalModules <- enumerateVisiblePackageModules
  Log.debug $ "Enumerated " <> show (length externalModules) <> " visible package modules."
  hscEnv <- GHC.getSession
  externalModulesSymbols <- liftIO $ forM externalModules $ getExternalModuleSymbols hscEnv
  Log.debug $ "Fetched symbols for " <> show (length externalModulesSymbols) <> " external modules."
  logModuleSymbolIssues externalModulesSymbols
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
  = ModuleSymbolsLoaded GHC.Module [Symbol]
  | ModuleSymbolsMissing GHC.Module
  | ModuleSymbolsFailed GHC.Module SomeException

getExternalModuleSymbols :: GHC.HscEnv -> GHC.Module -> IO ModuleSymbolsResult
getExternalModuleSymbols hsc_env mdl = do
  handle
    do \(e :: SomeException) -> pure (ModuleSymbolsFailed mdl e)
    do
      iface <- GHC.hscGetModuleInterface hsc_env mdl
      pure $
        ModuleSymbolsLoaded
          mdl
          [ Symbol
              { name = exportedName,
                visibility = Symbol'ExportedFrom [mdl]
              }
          | exportedName <- deduplicateNames (concatMap GHC.availNames (GHC.mi_exports iface))
          ]

getHomeModuleSymbols :: (MonadLore m) => GHC.Module -> m ModuleSymbolsResult
getHomeModuleSymbols mdl = do
  handle
    do \(e :: SomeException) -> pure (ModuleSymbolsFailed mdl e)
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
              exportedSymbols =
                [ Symbol
                    { name = exportedName,
                      visibility = Symbol'ExportedFrom [mdl]
                    }
                | exportedName <- Set.toList exportedNameSet
                ]
              unexportedSymbols =
                [ Symbol
                    { name = unexportedName,
                      visibility = Symbol'Unexported
                    }
                | unexportedName <- Set.toList unexportedNameSet
                ]
          pure (ModuleSymbolsLoaded mdl (exportedSymbols <> unexportedSymbols))
  where
    isDefinedInCurrentModule name =
      case GHC.nameModule_maybe name of
        Just module_ -> module_ == mdl
        Nothing -> False

lookupSymbolsInIndex :: Text -> SymbolsIndex -> [Symbol]
lookupSymbolsInIndex queryText (SymbolsIndex symbolsMap) =
  Map.findWithDefault [] queryText symbolsMap

buildSymbolsIndex :: [ModuleSymbolsResult] -> SymbolsIndex
buildSymbolsIndex moduleSymbols =
  SymbolsIndex $
    fmap toSymbols $
      foldl' insertModuleSymbols Map.empty moduleSymbols
  where
    insertModuleSymbols grouped = \case
      ModuleSymbolsLoaded _ symbols ->
        foldl' insertSymbol grouped symbols
      ModuleSymbolsMissing _ ->
        grouped
      ModuleSymbolsFailed _ _ ->
        grouped

    insertSymbol grouped symbol =
      Map.insertWith
        (Map.unionWith (<>))
        (T.pack (GHC.getOccString symbol.name))
        (Map.singleton symbol.name (visibilityExportedFrom symbol.visibility))
        grouped

    visibilityExportedFrom = \case
      Symbol'ExportedFrom modules_ -> modules_
      Symbol'Unexported -> []

    toSymbols =
      map (\(symbolName, modules) -> Symbol symbolName (toVisibility modules))
        . Map.toList

    toVisibility modules =
      case nub modules of
        [] -> Symbol'Unexported
        deduplicatedModules -> Symbol'ExportedFrom deduplicatedModules

deduplicateNames :: [GHC.Name] -> [GHC.Name]
deduplicateNames =
  Set.toList . Set.fromList

logPreparedSymbolsIndex :: (MonadLore m) => String -> SymbolsIndex -> m ()
logPreparedSymbolsIndex scope (SymbolsIndex symbolsMap) = do
  let symbolsCount = sum (map length (Map.elems symbolsMap))
  Log.debug $ "Collected " <> show symbolsCount <> " symbols from " <> scope <> "."
  Log.debug $ "Prepared symbols map with " <> show (Map.size symbolsMap) <> " unique symbol names for " <> scope <> "."

logModuleSymbolIssues :: (MonadLore m) => [ModuleSymbolsResult] -> m ()
logModuleSymbolIssues =
  mapM_ \case
    ModuleSymbolsLoaded _ _ ->
      pure ()
    ModuleSymbolsMissing module_ ->
      Log.warn $ "Module info not found for " <> show module_.moduleName
    ModuleSymbolsFailed module_ err ->
      Log.err $ "Failed to get module info for " <> show module_.moduleName <> ": " <> show err
