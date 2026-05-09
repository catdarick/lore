module Lore.Internal.Definition.Cache.CoreModuleFacts
  ( CoreModuleFactsCache (..),
    emptyCoreModuleFactsCache,
    lookupCoreModuleFactsCache,
    storeCoreModuleFactsCache,
    storeCoreModuleFactsCacheInContext,
    retainCoreModuleFactsCacheForLoadedModules,
    invalidateCoreModuleFactsCache,
  )
where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.Types (CoreModuleFactsCache (..))
import Lore.Internal.Definition.Types (MinimalCoreModuleFacts)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, modifyMVar_, readMVar)

emptyCoreModuleFactsCache :: CoreModuleFactsCache
emptyCoreModuleFactsCache =
  CoreModuleFactsCache Map.empty

lookupCoreModuleFactsCache :: (MonadLore m) => GHC.Module -> m (Maybe MinimalCoreModuleFacts)
lookupCoreModuleFactsCache homeModule = do
  cacheVar <- asks coreModuleFactsCacheVar
  CoreModuleFactsCache coreFactsByModule <- readMVar cacheVar
  pure (Map.lookup homeModule coreFactsByModule)

storeCoreModuleFactsCache :: (MonadLore m) => GHC.Module -> MinimalCoreModuleFacts -> m ()
storeCoreModuleFactsCache homeModule coreFacts = do
  sessionContext <- asks id
  liftIO (storeCoreModuleFactsCacheInContext sessionContext homeModule coreFacts)

storeCoreModuleFactsCacheInContext :: SessionContext -> GHC.Module -> MinimalCoreModuleFacts -> IO ()
storeCoreModuleFactsCacheInContext sessionContext homeModule coreFacts =
  modifyMVar_ (coreModuleFactsCacheVar sessionContext) \(CoreModuleFactsCache coreFactsByModule) ->
    evaluate (CoreModuleFactsCache (Map.insert homeModule coreFacts coreFactsByModule))

retainCoreModuleFactsCacheForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainCoreModuleFactsCacheForLoadedModules loadedModules = do
  cacheVar <- asks coreModuleFactsCacheVar
  modifyMVar cacheVar $ \(CoreModuleFactsCache coreFactsByModule) ->
    pure (CoreModuleFactsCache (Map.restrictKeys coreFactsByModule loadedModules), ())

invalidateCoreModuleFactsCache :: (MonadLore m) => m ()
invalidateCoreModuleFactsCache = do
  cacheVar <- asks coreModuleFactsCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptyCoreModuleFactsCache, ())
