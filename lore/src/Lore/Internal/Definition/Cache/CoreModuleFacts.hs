module Lore.Internal.Definition.Cache.CoreModuleFacts
  ( CoreModuleFactsCache (..),
    lookupCoreModuleFactsCache,
    storeCoreModuleFactsCacheInContext,
    retainCoreModuleFactsCacheForLoadedModules,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.Types (CoreModuleFactsCache (..))
import Lore.Internal.Definition.Types (MinimalCoreModuleFacts)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, modifyMVar_, readMVar)

lookupCoreModuleFactsCache :: (MonadLore m) => GHC.Module -> m (Maybe MinimalCoreModuleFacts)
lookupCoreModuleFactsCache homeModule = do
  cacheVar <- asks coreModuleFactsCacheVar
  CoreModuleFactsCache coreFactsByModule <- readMVar cacheVar
  pure (Map.lookup homeModule coreFactsByModule)

storeCoreModuleFactsCacheInContext ::
  SessionContext ->
  GHC.Module ->
  MinimalCoreModuleFacts ->
  IO ()
storeCoreModuleFactsCacheInContext sessionContext homeModule coreFacts0 = do
  coreFacts <- evaluate $ force coreFacts0
  modifyMVar_ (coreModuleFactsCacheVar sessionContext) \(CoreModuleFactsCache coreFactsByModule) -> do
    let !newCache =
          CoreModuleFactsCache (Map.insert homeModule coreFacts coreFactsByModule)
    pure newCache

retainCoreModuleFactsCacheForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainCoreModuleFactsCacheForLoadedModules loadedModules = do
  cacheVar <- asks coreModuleFactsCacheVar
  modifyMVar cacheVar $ \(CoreModuleFactsCache coreFactsByModule) ->
    pure (CoreModuleFactsCache (Map.restrictKeys coreFactsByModule loadedModules), ())
