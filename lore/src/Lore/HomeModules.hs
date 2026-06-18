module Lore.HomeModules
  ( HomeModulesLoadSummary (..),
    LoadHomeModulesResult (..),
    ProjectEnvironmentFailure (..),
    projectEnvironmentFailureMessage,
    projectEnvironmentFailureRequiresRestart,
    LoadHomeModulesOptions (..),
    defaultLoadHomeModulesOptions,
    lookupLastLoadHomeModulesResult,
    loadHomeModules,
    module Lore.HomeModules.CompilationGraph,
  )
where

import Lore.HomeModules.CompilationGraph
import Lore.Internal.HomeModules
  ( HomeModulesLoadSummary (..),
    LoadHomeModulesOptions (..),
    LoadHomeModulesResult (..),
    defaultLoadHomeModulesOptions,
    loadHomeModules,
    lookupLastLoadHomeModulesResultCache,
  )
import Lore.Internal.ProjectEnvironment.Types (ProjectEnvironmentFailure (..), projectEnvironmentFailureMessage, projectEnvironmentFailureRequiresRestart)
import Lore.Monad (MonadLore)

lookupLastLoadHomeModulesResult :: (MonadLore m) => m (Maybe LoadHomeModulesResult)
lookupLastLoadHomeModulesResult =
  lookupLastLoadHomeModulesResultCache
