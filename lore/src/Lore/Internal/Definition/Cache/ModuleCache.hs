module Lore.Internal.Definition.Cache.ModuleCache
  ( lookupModuleCache,
    storeModuleCache,
    retainModuleCache,
  )
where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.MVar (MVar)
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.Types (ModuleCache (..))
import UnliftIO (MonadUnliftIO, modifyMVar, modifyMVar_, readMVar)

lookupModuleCache ::
  (MonadIO m) =>
  GHC.Module ->
  MVar (ModuleCache a) ->
  m (Maybe a)
lookupModuleCache homeModule cacheVar = do
  ModuleCache factsByModule <- readMVar cacheVar
  pure (Map.lookup homeModule factsByModule)

storeModuleCache ::
  GHC.Module ->
  a ->
  MVar (ModuleCache a) ->
  IO ()
storeModuleCache homeModule facts cacheVar =
  modifyMVar_ cacheVar \(ModuleCache factsByModule) ->
    evaluate $
      forceModuleCacheSpine $
        ModuleCache (Map.insert homeModule facts factsByModule)

retainModuleCache ::
  (MonadUnliftIO m) =>
  Set.Set GHC.Module ->
  MVar (ModuleCache a) ->
  m ()
retainModuleCache loadedModules cacheVar =
  modifyMVar cacheVar \(ModuleCache factsByModule) -> do
    retainedCache <-
      liftIO $
        evaluate $
          forceModuleCacheSpine $
            ModuleCache (Map.restrictKeys factsByModule loadedModules)
    pure (retainedCache, ())

forceModuleCacheSpine :: ModuleCache a -> ModuleCache a
forceModuleCacheSpine cache@(ModuleCache factsByModule) =
  moduleMapSpineSize factsByModule `seq` cache

moduleMapSpineSize :: Map.Map GHC.Module a -> Int
moduleMapSpineSize =
  Map.foldlWithKey' (\count homeModule _ -> homeModule `seq` count + 1) 0
