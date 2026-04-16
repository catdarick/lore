module Lore.Lib.Force where

import Control.DeepSeq (NFData, force)
import UnliftIO (MonadUnliftIO, evaluate)

evaluateNF :: (MonadUnliftIO m, NFData a) => a -> m a
evaluateNF a = evaluate (force a)

evaluateNFM :: (MonadUnliftIO m, NFData a) => m a -> m a
evaluateNFM ma = ma >>= evaluateNF
