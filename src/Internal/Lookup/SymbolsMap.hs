{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
module Internal.Lookup.SymbolsMap (getSymbolsMap, invalidateSymbolsMapCache) where

import Control.Exception (Exception (toException), SomeException)
import Control.Monad (forM)
import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as GHC
import qualified Internal.Logger as Log
import Internal.Lookup.Types (ExportedSymbol (..), SymbolsMap (..))
import Monad (MonadLore)
import Session (SessionContext (..))
import UnliftIO (modifyMVar)

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
  homeModules <- enumerateHomeModules
  externalModules <- enumerateVisiblePackageModules
  let modules = homeModules <> externalModules
  namedSymbols <- forM modules \m -> do
    safeGetModuleInfo m >>= \case
      Left err -> do
        Log.err $ "Failed to get module info for " <> show m.moduleName <> ": " <> show err
        pure []
      Right Nothing -> do
        Log.warn $ "Module info not found for " <> show m.moduleName
        pure []
      Right (Just modInfo) -> do
        let names = GHC.modInfoExports modInfo
        pure [(T.pack (GHC.getOccString n), n, m) | n <- names]
  let grouped = buildGroupedMap (concat namedSymbols)
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
  mg <- GHC.getModuleGraph
  let summaries = GHC.mgModSummaries mg
      mods = map GHC.ms_mod summaries
  pure mods

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

safeGetModuleInfo ::
  (MonadLore m) =>
  GHC.Module ->
  m (Either SomeException (Maybe GHC.ModuleInfo))
safeGetModuleInfo mdl =
  GHC.handleSourceError
    (pure . Left . toException)
    (Right <$> GHC.getModuleInfo mdl)
