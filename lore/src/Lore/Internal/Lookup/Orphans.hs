module Lore.Internal.Lookup.Orphans
  ( collectIndexModules,
  )
where

import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Unit.Module.Deps as Deps
import qualified GHC.Unit.Module.ModIface as ModIface
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (tryAny)

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
