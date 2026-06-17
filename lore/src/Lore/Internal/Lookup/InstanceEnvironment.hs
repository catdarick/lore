module Lore.Internal.Lookup.InstanceEnvironment
  ( getCachedInstanceEnvironmentInputs,
    invalidateInstanceEnvironmentInputsCache,
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (catMaybes)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified GHC
import qualified GHC.Core.InstEnv as InstEnv
import Lore.Internal.Lookup.Cache.Types (InstanceEnvironmentInputsCache (..))
import Lore.Internal.Lookup.Orphans (collectIndexModules)
import Lore.Internal.Lookup.Types (InstanceEnvironmentInputs (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, tryAny)

getCachedInstanceEnvironmentInputs :: (MonadLore m) => m InstanceEnvironmentInputs
getCachedInstanceEnvironmentInputs = do
  cacheVar <- asks instanceEnvironmentInputsCacheVar
  modifyMVar cacheVar $ \cacheState ->
    case cacheState.cachedInstanceEnvironmentInputs of
      Just inputs ->
        pure (cacheState, inputs)
      Nothing -> do
        inputs <- prepareInstanceEnvironmentInputs
        pure (InstanceEnvironmentInputsCache (Just inputs), inputs)

invalidateInstanceEnvironmentInputsCache :: (MonadLore m) => m ()
invalidateInstanceEnvironmentInputsCache = do
  cacheVar <- asks instanceEnvironmentInputsCacheVar
  modifyMVar cacheVar $ \_ -> pure (InstanceEnvironmentInputsCache Nothing, ())

prepareInstanceEnvironmentInputs :: (MonadLore m) => m InstanceEnvironmentInputs
prepareInstanceEnvironmentInputs = do
  startedAt <- liftIO getCurrentTime
  Log.debug "Preparing shared instance environment inputs..."
  moduleGraph <- GHC.getModuleGraph
  let candidateModules = [GHC.ms_mod ms | ms <- GHC.mgModSummaries moduleGraph]
  loaded <- fmap catMaybes $
    forM candidateModules \module_ ->
      tryAny (GHC.getModuleInfo module_) >>= \case
        Right (Just info) ->
          pure (Just (module_, GHC.modInfoInstances info))
        Right Nothing ->
          pure Nothing
        Left err -> do
          Log.warn $
            "Skipping unloaded home module while preparing instance environment: "
              <> GHC.moduleNameString (GHC.moduleName module_)
              <> " ("
              <> show err
              <> ")"
          pure Nothing
  homeModulesReadyAt <- liftIO getCurrentTime
  let homeModules = map fst loaded
  visibleModules <- collectIndexModules homeModules
  visibleModulesReadyAt <- liftIO getCurrentTime
  let homeClassInstances = concatMap snd loaded
      !localInstEnv = InstEnv.extendInstEnvList InstEnv.emptyInstEnv homeClassInstances
  finishedAt <- liftIO getCurrentTime
  Log.debug $
    "Prepared shared instance environment inputs from "
      <> show (length homeModules)
      <> " loaded home modules, "
      <> show (length homeClassInstances)
      <> " home class instances, and "
      <> show (length visibleModules)
      <> " visible modules in "
      <> show (diffUTCTime finishedAt startedAt)
      <> " (home module inspection: "
      <> show (diffUTCTime homeModulesReadyAt startedAt)
      <> ", orphan closure: "
      <> show (diffUTCTime visibleModulesReadyAt homeModulesReadyAt)
      <> ", local instance environment: "
      <> show (diffUTCTime finishedAt visibleModulesReadyAt)
      <> ")."
  pure
    InstanceEnvironmentInputs
      { instanceEnvironmentLocalInstEnv = localInstEnv,
        instanceEnvironmentVisibleModules = visibleModules
      }
