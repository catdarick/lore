{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}
module Lore.Internal.Lookup.NameToInstances
  ( getNameToInstancesIndex,
    invalidateNameToInstancesIndex,
  )
where

import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Lookup.Types (NameToInstancesIndex (..))
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar)

getNameToInstancesIndex :: (MonadLore m) => m NameToInstancesIndex
getNameToInstancesIndex = do
  cacheVar <- asks nameToInstancesIndexCache
  modifyMVar cacheVar $ \case
    Just nameToInstancesIndex -> pure (Just nameToInstancesIndex, nameToInstancesIndex)
    Nothing -> do
      nameToInstancesIndex <- prepareNameToInstancesIndex
      pure (Just nameToInstancesIndex, nameToInstancesIndex)

invalidateNameToInstancesIndex :: (MonadLore m) => m ()
invalidateNameToInstancesIndex = do
  cacheVar <- asks nameToInstancesIndexCache
  modifyMVar cacheVar $ \_ -> pure (Nothing, ())

prepareNameToInstancesIndex :: (MonadLore m) => m NameToInstancesIndex
prepareNameToInstancesIndex = do
  moduleGraph <- GHC.getModuleGraph
  let mods = [GHC.ms_mod ms | ms <- GHC.mgModSummaries moduleGraph]
  (_, mIndex) <- GHC.getNameToInstancesIndex mods (Just mods)

  pure $ NameToInstancesIndex (fromMaybe GHC.emptyNameEnv mIndex)
