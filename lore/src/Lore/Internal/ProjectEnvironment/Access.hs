module Lore.Internal.ProjectEnvironment.Access
  ( getProjectEnvironment,
    requireProjectEnvironment,
    getProjectPackages,
  )
where

import Lore.Internal.Package.Types (PackageData)
import Lore.Internal.ProjectEnvironment.Refresh (refreshProjectEnvironment)
import Lore.Internal.ProjectEnvironment.Types (ProjectEnvironmentFailure, ProjectEnvironmentRefresh (..), ProjectEnvironmentState (..), projectEnvironmentFailureMessage)
import Lore.Monad (MonadLore)
import UnliftIO.Exception (throwString)

getProjectEnvironment :: (MonadLore m) => m (Either ProjectEnvironmentFailure ProjectEnvironmentState)
getProjectEnvironment = do
  refreshResult <- refreshProjectEnvironment
  pure (fmap refreshedProjectEnvironment refreshResult)

requireProjectEnvironment :: (MonadLore m) => m ProjectEnvironmentState
requireProjectEnvironment = do
  environmentResult <- getProjectEnvironment
  case environmentResult of
    Left failure -> throwString (projectEnvironmentFailureMessage failure)
    Right environment -> pure environment

getProjectPackages :: (MonadLore m) => m [PackageData]
getProjectPackages = do
  environment <- requireProjectEnvironment
  pure environment.projectEnvironmentPackages
