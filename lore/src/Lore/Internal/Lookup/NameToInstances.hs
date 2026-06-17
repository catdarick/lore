module Lore.Internal.Lookup.NameToInstances
  ( getCachedNameToInstancesIndex,
    invalidateNameToInstancesIndexCache,
  )
where

import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import qualified GHC
import qualified GHC.Plugins as GHC.Plugins
import Lore.Internal.Lookup.Cache.Types (NameToInstancesIndexCache (..))
import Lore.Internal.Lookup.InstanceEnvironment (getCachedInstanceEnvironmentInputs)
import Lore.Internal.Lookup.Types (InstanceEnvironmentInputs (..), NameToInstancesIndex (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar)

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
  instanceEnvironmentInputs <- getCachedInstanceEnvironmentInputs
  let indexModules = instanceEnvironmentInputs.instanceEnvironmentVisibleModules
  (_, mIndex) <- GHC.getNameToInstancesIndex indexModules (Just indexModules)
  Log.debug $ "Name-to-instances index prepared with " <> show (GHC.Plugins.sizeUFM (fromMaybe GHC.Plugins.emptyNameEnv mIndex)) <> " entries."
  pure $ NameToInstancesIndex (fromMaybe GHC.Plugins.emptyNameEnv mIndex)
