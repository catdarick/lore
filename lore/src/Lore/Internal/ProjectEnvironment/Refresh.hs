module Lore.Internal.ProjectEnvironment.Refresh
  ( refreshProjectEnvironment,
    refreshProjectEnvironmentWith,
    ProjectEnvironmentRefreshRunners (..),
    defaultProjectEnvironmentRefreshRunners,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Lore.Internal.BuildTool.Dependencies (prepareProjectDependencies)
import Lore.Internal.Ghc.PackageEnvironment.Probe (captureGhcEnvironment)
import Lore.Internal.Ghc.PackageEnvironment.Resolve (renderPackageResolutionError, resolveDependencyPackageEnvironment)
import Lore.Internal.Ghc.PackageEnvironment.Types (CapturedGhcEnvironment (..), PackageEnvironmentSnapshot)
import Lore.Internal.ProjectEnvironment.Prepare (prepareProjectDescription)
import Lore.Internal.ProjectEnvironment.Types (PreparedProjectDescription (..), ProjectEnvironmentFailure (..), ProjectEnvironmentRefresh (..), ProjectEnvironmentState (..))
import Lore.Internal.ProjectProvider (ProjectProvider)
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import qualified UnliftIO.MVar as MVar

data ProjectEnvironmentRefreshRunners m = ProjectEnvironmentRefreshRunners
  { refreshRunnerPrepareDescription :: m (Either ProjectEnvironmentFailure PreparedProjectDescription),
    refreshRunnerPrepareDependencies :: ProjectProvider -> FilePath -> IO (Either String ()),
    refreshRunnerCaptureEnvironment :: ProjectProvider -> FilePath -> IO (Either String CapturedGhcEnvironment)
  }

defaultProjectEnvironmentRefreshRunners :: (MonadLore m) => ProjectEnvironmentRefreshRunners m
defaultProjectEnvironmentRefreshRunners =
  ProjectEnvironmentRefreshRunners
    { refreshRunnerPrepareDescription = prepareProjectDescription,
      refreshRunnerPrepareDependencies = prepareProjectDependencies,
      refreshRunnerCaptureEnvironment = captureGhcEnvironment
    }

refreshProjectEnvironment :: (MonadLore m) => m (Either ProjectEnvironmentFailure ProjectEnvironmentRefresh)
refreshProjectEnvironment =
  refreshProjectEnvironmentWith defaultProjectEnvironmentRefreshRunners

refreshProjectEnvironmentWith :: (MonadLore m) => ProjectEnvironmentRefreshRunners m -> m (Either ProjectEnvironmentFailure ProjectEnvironmentRefresh)
refreshProjectEnvironmentWith runners = do
  stateVar <- asks projectEnvironmentStateVar
  MVar.modifyMVar stateVar \maybePreviousState -> do
    refreshResult <- runRefresh runners maybePreviousState
    pure case refreshResult of
      Left failure -> (maybePreviousState, Left failure)
      Right refresh -> (Just refresh.refreshedProjectEnvironment, Right refresh)

runRefresh :: (MonadLore m) => ProjectEnvironmentRefreshRunners m -> Maybe ProjectEnvironmentState -> m (Either ProjectEnvironmentFailure ProjectEnvironmentRefresh)
runRefresh runners maybePreviousState = do
  preparedResult <- runners.refreshRunnerPrepareDescription
  case preparedResult of
    Left failure -> pure (Left failure)
    Right prepared -> do
      let configChanged = maybe True ((/= prepared.preparedConfigurationSnapshot) . (.projectEnvironmentConfigurationSnapshot)) maybePreviousState
          previousCanResolve =
            case maybePreviousState of
              Nothing -> False
              Just previous ->
                case resolveDependencyPackageEnvironment previous.projectEnvironmentCapturedPackages prepared.preparedRequiredExternalDependencies of
                  Left _ -> False
                  Right _ -> True
          preparationRequired = configChanged || not previousCanResolve
      Log.debug $ "Project dependency configuration changed: " <> show configChanged
      Log.debug $ "Dependency preparation required: " <> show preparationRequired
      if preparationRequired
        then prepareAndCapture runners maybePreviousState
        else reusePrevious runners prepared maybePreviousState

prepareAndCapture :: (MonadLore m) => ProjectEnvironmentRefreshRunners m -> Maybe ProjectEnvironmentState -> m (Either ProjectEnvironmentFailure ProjectEnvironmentRefresh)
prepareAndCapture runners maybePreviousState = do
  provider <- asks projectProvider
  root <- asks projectRoot
  stableToolchain <- asks ghcToolchain
  prepResult <- liftIO $ runners.refreshRunnerPrepareDependencies provider root
  case prepResult of
    Left failure -> pure (Left (ProjectEnvironmentFailed failure))
    Right () -> do
      postBuildPreparedResult <- runners.refreshRunnerPrepareDescription
      case postBuildPreparedResult of
        Left failure -> pure (Left failure)
        Right postBuildPrepared -> do
          capturedResult <- liftIO $ runners.refreshRunnerCaptureEnvironment provider root
          case capturedResult of
            Left err -> pure (Left (ProjectEnvironmentFailed err))
            Right captured
              | captured.capturedGhcToolchain /= stableToolchain -> do
                  Log.err "Project resolved to a different GHC toolchain; Lore restart is required."
                  pure (Left (ProjectEnvironmentRestartRequired "The project now resolves to a different GHC toolchain. Restart Lore."))
              | otherwise ->
                  buildState postBuildPrepared captured.capturedPackageEnvironment maybePreviousState

reusePrevious :: (MonadLore m) => ProjectEnvironmentRefreshRunners m -> PreparedProjectDescription -> Maybe ProjectEnvironmentState -> m (Either ProjectEnvironmentFailure ProjectEnvironmentRefresh)
reusePrevious runners prepared maybePreviousState =
  case maybePreviousState of
    Nothing -> prepareAndCapture runners Nothing
    Just previous ->
      buildState prepared previous.projectEnvironmentCapturedPackages maybePreviousState

buildState :: (MonadLore m) => PreparedProjectDescription -> PackageEnvironmentSnapshot -> Maybe ProjectEnvironmentState -> m (Either ProjectEnvironmentFailure ProjectEnvironmentRefresh)
buildState prepared packageSnapshot maybePreviousState =
  case resolveDependencyPackageEnvironment packageSnapshot prepared.preparedRequiredExternalDependencies of
    Left resolutionError -> do
      Log.err $ "Failed to resolve required external package: " <> show resolutionError
      pure (Left (ProjectEnvironmentFailed (renderPackageResolutionError resolutionError)))
    Right resolvedEnvironment -> do
      let newState =
            ProjectEnvironmentState
              { projectEnvironmentPackageRoots = prepared.preparedPackageRoots,
                projectEnvironmentCabalFiles = prepared.preparedCabalFiles,
                projectEnvironmentPackages = prepared.preparedPackages,
                projectEnvironmentRequiredDependencies = prepared.preparedRequiredExternalDependencies,
                projectEnvironmentConfigurationSnapshot = prepared.preparedConfigurationSnapshot,
                projectEnvironmentCapturedPackages = packageSnapshot,
                projectEnvironmentResolvedPackages = resolvedEnvironment
              }
          environmentChanged =
            maybe True ((/= resolvedEnvironment) . (.projectEnvironmentResolvedPackages)) maybePreviousState
      Log.debug $ "Package environment changed: " <> show environmentChanged
      pure (Right ProjectEnvironmentRefresh {refreshedProjectEnvironment = newState, projectEnvironmentChanged = environmentChanged})
