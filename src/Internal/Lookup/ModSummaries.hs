module Internal.Lookup.ModSummaries (getModSummaries, invalidateModSummaries) where

import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import qualified GHC
import Internal.Lookup.Types (ModSummaries (..))
import Monad (MonadLore)
import Session (SessionContext (..))
import UnliftIO (modifyMVar)

getModSummaries :: (MonadLore m) => m ModSummaries
getModSummaries = do
  cacheVar <- asks modSummariesCache
  modifyMVar cacheVar $ \case
    Just modSummaries -> pure (Just modSummaries, modSummaries)
    Nothing -> do
      modSummaries <- prepareModSummaries
      pure (Just modSummaries, modSummaries)

invalidateModSummaries :: (MonadLore m) => m ()
invalidateModSummaries = do
  cacheVar <- asks modSummariesCache
  modifyMVar cacheVar $ \_ -> pure (Nothing, ())

prepareModSummaries :: (MonadLore m) => m ModSummaries
prepareModSummaries = do
  moduleGraph <- GHC.getModuleGraph
  let modSummariesMap = Map.fromList [(GHC.ms_mod ms, ms) | ms <- GHC.mgModSummaries moduleGraph]
  pure $ ModSummaries modSummariesMap
