module Lore.Targets
  ( LoadTargetsResult (..),
    LoadTargetsOptions (..),
    defaultLoadTargetsOptions,
    lookupLastLoadTargetsResult,
    loadTargets,
  )
where

import Lore.Internal.Targets
  ( LoadTargetsOptions (..),
    LoadTargetsResult (..),
    defaultLoadTargetsOptions,
    loadTargets,
    lookupLastLoadTargetsResultCache,
  )
import Lore.Monad (MonadLore)

lookupLastLoadTargetsResult :: (MonadLore m) => m (Maybe LoadTargetsResult)
lookupLastLoadTargetsResult =
  lookupLastLoadTargetsResultCache
