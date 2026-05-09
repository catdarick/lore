module Lore.Internal.Definition.Timing
  ( logTimedSectionStart,
    logTimedSectionEnd,
    withTimedSection,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (finally)

logTimedSectionStart :: (MonadLore m) => String -> m ()
logTimedSectionStart label = do
  now <- liftIO getCurrentTime
  Log.debug $ "Starting " <> label <> " at " <> show now

logTimedSectionEnd :: (MonadLore m) => String -> m ()
logTimedSectionEnd label = do
  now <- liftIO getCurrentTime
  Log.debug $ "Finished " <> label <> " at " <> show now

withTimedSection :: (MonadLore m) => String -> m a -> m a
withTimedSection label action = do
  logTimedSectionStart label
  action `finally` logTimedSectionEnd label
