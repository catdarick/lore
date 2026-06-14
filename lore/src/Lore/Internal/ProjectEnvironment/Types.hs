module Lore.Internal.ProjectEnvironment.Types
  ( ProjectConfigurationSnapshot (..),
    PreparedProjectDescription (..),
    ProjectEnvironmentState (..),
    ProjectEnvironmentRefresh (..),
    ProjectEnvironmentFailure (..),
    projectEnvironmentFailureMessage,
    projectEnvironmentFailureRequiresRestart,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( PackageEnvironmentSnapshot,
    ResolvedPackageEnvironment,
  )
import Lore.Internal.Package.Root (PackageRoot)
import Lore.Internal.Package.Types (ComponentIdentity, DependencyFingerprint, PackageData)
import Lore.Internal.ProjectProvider (ProjectProvider)

data ProjectConfigurationSnapshot = ProjectConfigurationSnapshot
  { projectConfigurationProvider :: ProjectProvider,
    projectConfigurationPackageRoots :: [PackageRoot],
    projectConfigurationDependencies :: Map.Map ComponentIdentity (Set.Set DependencyFingerprint),
    projectConfigurationProviderFiles :: [(FilePath, BS.ByteString)]
  }
  deriving (Eq, Show)

data PreparedProjectDescription = PreparedProjectDescription
  { preparedPackageRoots :: [PackageRoot],
    preparedCabalFiles :: [FilePath],
    preparedPackages :: [PackageData],
    preparedRequiredExternalDependencies :: Set.Set String,
    preparedConfigurationSnapshot :: ProjectConfigurationSnapshot
  }
  deriving (Show)

data ProjectEnvironmentState = ProjectEnvironmentState
  { projectEnvironmentPackageRoots :: [PackageRoot],
    projectEnvironmentCabalFiles :: [FilePath],
    projectEnvironmentPackages :: [PackageData],
    projectEnvironmentRequiredDependencies :: Set.Set String,
    projectEnvironmentConfigurationSnapshot :: ProjectConfigurationSnapshot,
    projectEnvironmentCapturedPackages :: PackageEnvironmentSnapshot,
    projectEnvironmentResolvedPackages :: ResolvedPackageEnvironment
  }
  deriving (Show)

data ProjectEnvironmentRefresh = ProjectEnvironmentRefresh
  { refreshedProjectEnvironment :: ProjectEnvironmentState,
    projectEnvironmentChanged :: Bool
  }
  deriving (Show)

data ProjectEnvironmentFailure
  = ProjectEnvironmentFailed String
  | ProjectEnvironmentRestartRequired String
  deriving (Eq, Show)

projectEnvironmentFailureMessage :: ProjectEnvironmentFailure -> String
projectEnvironmentFailureMessage = \case
  ProjectEnvironmentFailed message -> message
  ProjectEnvironmentRestartRequired message -> message

projectEnvironmentFailureRequiresRestart :: ProjectEnvironmentFailure -> Bool
projectEnvironmentFailureRequiresRestart = \case
  ProjectEnvironmentFailed _ -> False
  ProjectEnvironmentRestartRequired _ -> True
