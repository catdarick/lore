module Lore.Internal.Definition.Cache.CoreModuleFacts
  ( CoreModuleFactsCache,
    lookupCoreModuleFactsCache,
    storeCoreModuleFactsCacheInContext,
    retainCoreModuleFactsCacheForLoadedModules,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Reader (asks)
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.ModuleCache (lookupModuleCache, retainModuleCache, storeModuleCache)
import Lore.Internal.Definition.Cache.Types (CoreModuleFactsCache)
import Lore.Internal.Definition.Types (MinimalCoreModuleFacts)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)

lookupCoreModuleFactsCache :: (MonadLore m) => GHC.Module -> m (Maybe MinimalCoreModuleFacts)
lookupCoreModuleFactsCache homeModule = do
  cacheVar <- asks coreModuleFactsCacheVar
  lookupModuleCache homeModule cacheVar

storeCoreModuleFactsCacheInContext ::
  SessionContext ->
  GHC.Module ->
  MinimalCoreModuleFacts ->
  IO ()
storeCoreModuleFactsCacheInContext sessionContext homeModule coreFacts0 = do
  coreFacts <- evaluate $ force coreFacts0
  storeModuleCache homeModule coreFacts (coreModuleFactsCacheVar sessionContext)

retainCoreModuleFactsCacheForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainCoreModuleFactsCacheForLoadedModules loadedModules = do
  cacheVar <- asks coreModuleFactsCacheVar
  retainModuleCache loadedModules cacheVar
