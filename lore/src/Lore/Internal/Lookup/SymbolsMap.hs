module Lore.Internal.Lookup.SymbolsMap
  ( getSymbolsMap,
    invalidateSymbolsMapCache,
  )
where

import Control.Monad (forM)
import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Types (ExportedSymbol (..), ModSummaries (..), SymbolsMap (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (SomeException, evaluate, modifyMVar, tryAny)

getSymbolsMap :: (MonadLore m) => m SymbolsMap
getSymbolsMap = do
  cacheVar <- asks externalPackagesSymbolsCache
  modifyMVar cacheVar $ \case
    Just symbolsMap -> pure (Just symbolsMap, symbolsMap)
    Nothing -> do
      symbolsMap <- prepareSymbolsMap
      pure (Just symbolsMap, symbolsMap)

invalidateSymbolsMapCache :: (MonadLore m) => m ()
invalidateSymbolsMapCache = do
  cacheVar <- asks externalPackagesSymbolsCache
  modifyMVar cacheVar $ \_ -> pure (Nothing, ())

prepareSymbolsMap :: (MonadLore m) => m SymbolsMap
prepareSymbolsMap = do
  Log.debug "Preparing symbols map for all visible modules..."
  homeModules <- enumerateHomeModules
  Log.debug $ "Enumerated " <> show (length homeModules) <> " home modules."
  externalModules <- enumerateVisiblePackageModules
  Log.debug $ "Enumerated " <> show (length externalModules) <> " visible package modules."
  let modules = homeModules <> externalModules
  namedSymbols <- forM modules \m -> do
    safeGetModuleExports m >>= \case
      ModuleExportsFailed err -> do
        Log.err $ "Failed to get module info for " <> show m.moduleName <> ": " <> show err
        pure []
      ModuleExportsMissing -> do
        Log.warn $ "Module info not found for " <> show m.moduleName
        pure []
      ModuleExportsLoaded names -> do
        pure [(T.pack (GHC.getOccString n), n, m) | n <- names]
  Log.debug $ "Collected " <> show (length (concat namedSymbols)) <> " exported symbols from all visible modules."
  let grouped = buildGroupedMap (concat namedSymbols)
  Log.debug $ "Prepared symbols map with " <> show (Map.size grouped) <> " unique symbol names."
  pure $ SymbolsMap $ fmap toExportedSymbols grouped
  where
    buildGroupedMap :: [(Text, GHC.Name, GHC.Module)] -> Map.Map Text (Map.Map GHC.Name [GHC.Module])
    buildGroupedMap =
      Map.fromListWith (Map.unionWith (<>))
        . map toSingletonEntry
      where
        toSingletonEntry (symbolName, exportedName, exportedFromModule) =
          ( symbolName,
            Map.singleton exportedName [exportedFromModule]
          )
    toExportedSymbols :: Map.Map GHC.Name [GHC.Module] -> [ExportedSymbol]
    toExportedSymbols =
      map (\(exportedName, modules) -> ExportedSymbol exportedName modules)
        . Map.toList

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
  = ModuleExportsLoaded [GHC.Name]
  | ModuleExportsMissing
  | ModuleExportsFailed SomeException

safeGetModuleExports ::
  (MonadLore m) =>
  GHC.Module ->
  m ModuleExportsResult
safeGetModuleExports mdl = do
  tryAny
    ( do
        maybeInfo <- GHC.getModuleInfo mdl
        case maybeInfo of
          Nothing ->
            pure ModuleExportsMissing
          Just modInfo -> do
            let exportedNames = GHC.modInfoExports modInfo
            _ <- evaluate (length exportedNames)
            pure (ModuleExportsLoaded exportedNames)
    )
    >>= \case
      Left err -> pure (ModuleExportsFailed err)
      Right result -> pure result
