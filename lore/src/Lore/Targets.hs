module Lore.Targets
  ( LoadTargetsResult (..),
    LoadTargetsOptions (..),
    defaultLoadTargetsOptions,
    lookupLastLoadTargetsResult,
    loadTargets,
    retainUnresolvedRollback,
  )
where

import Lore.Internal.Targets
  ( LoadTargetsOptions (..),
    LoadTargetsResult (..),
    defaultLoadTargetsOptions,
    loadTargets,
    lookupLastLoadTargetsResultCache,
    retainUnresolvedRollback,
  )
import Lore.Monad (MonadLore)

lookupLastLoadTargetsResult :: (MonadLore m) => m (Maybe LoadTargetsResult)
lookupLastLoadTargetsResult =
  lookupLastLoadTargetsResultCache
