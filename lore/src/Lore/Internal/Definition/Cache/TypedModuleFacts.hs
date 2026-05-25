module Lore.Internal.Definition.Cache.TypedModuleFacts
  ( TypedModuleFactsCache (..),
    lookupTypedModuleFactsCache,
    lookupTypedModuleFactsCacheInContext,
    storeTypedModuleFactsCacheInContext,
    retainTypedModuleFactsCacheForLoadedModules,
  )
where

import Control.Exception (evaluate)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.Types (TypedModuleFactsCache (..))
import Lore.Internal.Definition.Types (MinimalTypedModuleFacts)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, modifyMVar_, readMVar)

lookupTypedModuleFactsCache :: (MonadLore m) => GHC.Module -> m (Maybe MinimalTypedModuleFacts)
lookupTypedModuleFactsCache homeModule = do
  sessionContext <- asks id
  readMVar (typedModuleFactsCacheVar sessionContext) >>= \(TypedModuleFactsCache typedFactsByModule) ->
    pure (Map.lookup homeModule typedFactsByModule)

lookupTypedModuleFactsCacheInContext :: SessionContext -> GHC.Module -> IO (Maybe MinimalTypedModuleFacts)
lookupTypedModuleFactsCacheInContext sessionContext homeModule = do
  TypedModuleFactsCache typedFactsByModule <- readMVar (typedModuleFactsCacheVar sessionContext)
  pure (Map.lookup homeModule typedFactsByModule)

storeTypedModuleFactsCacheInContext :: SessionContext -> GHC.Module -> MinimalTypedModuleFacts -> IO ()
storeTypedModuleFactsCacheInContext sessionContext homeModule typedFacts =
  modifyMVar_ (typedModuleFactsCacheVar sessionContext) \(TypedModuleFactsCache typedFactsByModule) ->
    evaluate (TypedModuleFactsCache (Map.insert homeModule typedFacts typedFactsByModule))

retainTypedModuleFactsCacheForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainTypedModuleFactsCacheForLoadedModules loadedModules = do
  cacheVar <- asks typedModuleFactsCacheVar
  modifyMVar cacheVar $ \(TypedModuleFactsCache typedFactsByModule) ->
    pure (TypedModuleFactsCache (Map.restrictKeys typedFactsByModule loadedModules), ())
