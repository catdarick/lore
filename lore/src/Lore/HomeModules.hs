module Lore.HomeModules
  ( LoadHomeModulesResult (..),
    LoadHomeModulesOptions (..),
    defaultLoadHomeModulesOptions,
    lookupLastLoadHomeModulesResult,
    loadHomeModules,
  )
where

import Lore.Internal.HomeModules
  ( LoadHomeModulesOptions (..),
    LoadHomeModulesResult (..),
    defaultLoadHomeModulesOptions,
    loadHomeModules,
    lookupLastLoadHomeModulesResultCache,
  )
import Lore.Monad (MonadLore)

lookupLastLoadHomeModulesResult :: (MonadLore m) => m (Maybe LoadHomeModulesResult)
lookupLastLoadHomeModulesResult =
  lookupLastLoadHomeModulesResultCache
