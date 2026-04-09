{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}
module Lore.Internal.Lookup.NameToInstances
  ( getNameToInstancesIndex,
    invalidateNameToInstancesIndex,
  )
where

import Control.Monad.Reader (asks)
import Data.Maybe (catMaybes, fromMaybe)
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Lookup.Types (NameToInstancesIndex (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, tryAny)

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
  Log.debug "Preparing name-to-instances index..."
  moduleGraph <- GHC.getModuleGraph
  let candidateMods = [GHC.ms_mod ms | ms <- GHC.mgModSummaries moduleGraph]
  Log.debug $ "Preparing name-to-instances index. There are " <> show (length candidateMods) <> " candidate modules to consider."
  mods <- catMaybes <$> mapM keepLoadedModule candidateMods
  Log.debug $ "Loaded " <> show (length mods) <> " modules for name-to-instances index."
  (_, mIndex) <- GHC.getNameToInstancesIndex mods (Just mods)
  Log.debug $ "Name-to-instances index prepared with " <> show (GHC.sizeUFM (fromMaybe GHC.emptyNameEnv mIndex)) <> " entries."
  pure $ NameToInstancesIndex (fromMaybe GHC.emptyNameEnv mIndex)
  where
    keepLoadedModule mod' =
      tryAny (GHC.getModuleInfo mod') >>= \case
        Right (Just _) ->
          pure (Just mod')
        Right Nothing ->
          pure Nothing
        Left err -> do
          Log.warn $
            "Skipping unloaded module while building name-to-instances index: "
              <> GHC.moduleNameString (GHC.moduleName mod')
              <> " ("
              <> show err
              <> ")"
          pure Nothing
