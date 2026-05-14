module Lore.Internal.Lookup.NameToInstances
  ( getCachedNameToInstancesIndex,
    invalidateNameToInstancesIndexCache,
  )
where

import Control.Monad.Reader (asks)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Plugins as GHC.Plugins
import qualified GHC.Unit.Module.Deps as Deps
import qualified GHC.Unit.Module.ModIface as ModIface
import Lore.Internal.Lookup.Cache.Types (NameToInstancesIndexCache (..))
import Lore.Internal.Lookup.Types (NameToInstancesIndex (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, tryAny)

getCachedNameToInstancesIndex :: (MonadLore m) => m NameToInstancesIndex
getCachedNameToInstancesIndex = do
  cacheVar <- asks nameToInstancesIndexCacheVar
  modifyMVar cacheVar $ \cacheState ->
    case cacheState.cachedNameToInstancesIndex of
      Just nameToInstancesIndex -> pure (cacheState, nameToInstancesIndex)
      Nothing -> do
        nameToInstancesIndex <- prepareNameToInstancesIndex
        pure (NameToInstancesIndexCache (Just nameToInstancesIndex), nameToInstancesIndex)

invalidateNameToInstancesIndexCache :: (MonadLore m) => m ()
invalidateNameToInstancesIndexCache = do
  cacheVar <- asks nameToInstancesIndexCacheVar
  modifyMVar cacheVar $ \_ -> pure (NameToInstancesIndexCache Nothing, ())

prepareNameToInstancesIndex :: (MonadLore m) => m NameToInstancesIndex
prepareNameToInstancesIndex = do
  Log.debug "Preparing name-to-instances index..."
  moduleGraph <- GHC.getModuleGraph
  let candidateMods = [GHC.ms_mod ms | ms <- GHC.mgModSummaries moduleGraph]
  Log.debug $ "Preparing name-to-instances index. There are " <> show (length candidateMods) <> " candidate modules to consider."
  loadedHomeMods <- catMaybes <$> mapM keepLoadedModule candidateMods
  Log.debug $ "Loaded " <> show (length loadedHomeMods) <> " home modules for name-to-instances index."
  indexMods <- collectIndexModules loadedHomeMods
  Log.debug $ "Expanded to " <> show (length indexMods) <> " modules after including orphan dependencies."
  (_, mIndex) <- GHC.getNameToInstancesIndex indexMods (Just indexMods)
  Log.debug $ "Name-to-instances index prepared with " <> show (GHC.Plugins.sizeUFM (fromMaybe GHC.Plugins.emptyNameEnv mIndex)) <> " entries."
  pure $ NameToInstancesIndex (fromMaybe GHC.Plugins.emptyNameEnv mIndex)
  where
    keepLoadedModule module_ =
      tryAny (GHC.getModuleInfo module_) >>= \case
        Right (Just _) ->
          pure (Just module_)
        Right Nothing ->
          pure Nothing
        Left err -> do
          Log.warn $
            "Skipping unloaded home module while building name-to-instances index: "
              <> GHC.moduleNameString (GHC.moduleName module_)
              <> " ("
              <> show err
              <> ")"
          pure Nothing

collectIndexModules :: (MonadLore m) => [GHC.Module] -> m [GHC.Module]
collectIndexModules seedModules =
  go (Set.fromList seedModules) seedModules
  where
    go seenModules [] =
      pure (Set.toList seenModules)
    go seenModules (module_ : pendingModules) = do
      orphanDeps <- orphanDependenciesForModule module_
      let newlyDiscoveredModules = filter (`Set.notMember` seenModules) orphanDeps
          seenModules' = foldr Set.insert seenModules newlyDiscoveredModules
      go seenModules' (pendingModules <> newlyDiscoveredModules)

orphanDependenciesForModule :: (MonadLore m) => GHC.Module -> m [GHC.Module]
orphanDependenciesForModule module_ =
  tryAny (GHC.getModuleInfo module_) >>= \case
    Right Nothing ->
      pure []
    Right (Just moduleInfo) ->
      case GHC.modInfoIface moduleInfo of
        Nothing ->
          pure []
        Just iface ->
          let dependencies = ModIface.mi_deps iface
           in pure (Deps.dep_orphs dependencies <> Deps.dep_finsts dependencies)
    Left err -> do
      Log.warn $
        "Could not inspect module dependencies while expanding orphan closure: "
          <> GHC.moduleNameString (GHC.moduleName module_)
          <> " ("
          <> show err
          <> ")"
      pure []
