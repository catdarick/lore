module Lore.Internal.Definition.Cache.TypedModuleFacts
  ( TypedModuleFactsCache,
    lookupTypedModuleFactsCache,
    lookupTypedModuleFactsCacheInContext,
    storeTypedModuleFactsCacheInContext,
    retainTypedModuleFactsCacheForLoadedModules,
  )
where

import Control.Monad.Reader (asks)
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.ModuleCache (lookupModuleCache, retainModuleCache, storeModuleCache)
import Lore.Internal.Definition.Cache.Types (TypedModuleFactsCache)
import Lore.Internal.Definition.Types (MinimalTypedModuleFacts)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)

lookupTypedModuleFactsCache :: (MonadLore m) => GHC.Module -> m (Maybe MinimalTypedModuleFacts)
lookupTypedModuleFactsCache homeModule = do
  sessionContext <- asks id
  lookupModuleCache homeModule (typedModuleFactsCacheVar sessionContext)

lookupTypedModuleFactsCacheInContext :: SessionContext -> GHC.Module -> IO (Maybe MinimalTypedModuleFacts)
lookupTypedModuleFactsCacheInContext sessionContext homeModule = do
  lookupModuleCache homeModule (typedModuleFactsCacheVar sessionContext)

storeTypedModuleFactsCacheInContext :: SessionContext -> GHC.Module -> MinimalTypedModuleFacts -> IO ()
storeTypedModuleFactsCacheInContext sessionContext homeModule typedFacts =
  storeModuleCache homeModule typedFacts (typedModuleFactsCacheVar sessionContext)

retainTypedModuleFactsCacheForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainTypedModuleFactsCacheForLoadedModules loadedModules = do
  cacheVar <- asks typedModuleFactsCacheVar
  retainModuleCache loadedModules cacheVar
