module Lore.Internal.ProjectEnvironment.Prepare
  ( prepareProjectDescription,
    loadProviderDependencyInputs,
    prepareProjectDescriptionIO,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Version (Version)
import Lore.Internal.Ghc.PackageEnvironment.Types (GhcToolchain (..))
import Lore.Internal.Package (preparePackagesIO)
import Lore.Internal.Package.Types (ComponentData (..), ComponentIdentity (..), DependencyFingerprint, PackageData (..))
import Lore.Internal.ProjectEnvironment.Types (PreparedProjectDescription (..), ProjectConfigurationSnapshot (..), ProjectEnvironmentFailure (..))
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import Lore.Internal.ProjectProvider.Ops (providerDependencyInputPaths, providerMaterializeRunner)
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.SourceText (relativeSourcePath)
import Lore.Monad (MonadLore)
import System.Directory (doesFileExist)
import System.FilePath (normalise, (</>))

prepareProjectDescription :: (MonadLore m) => m (Either ProjectEnvironmentFailure PreparedProjectDescription)
prepareProjectDescription = do
  provider <- asks projectProvider
  root <- asks projectRoot
  toolchain <- asks ghcToolchain
  liftIO $ prepareProjectDescriptionIO provider root toolchain.ghcToolchainCompilerVersion

prepareProjectDescriptionIO ::
  ProjectProvider ->
  FilePath ->
  Version ->
  IO (Either ProjectEnvironmentFailure PreparedProjectDescription)
prepareProjectDescriptionIO provider root ghcVersion = do
  packagesResult <-
    preparePackagesIO
      (providerMaterializeRunner provider)
      (const (pure ()))
      (relativeSourcePath root)
      provider
      root
      ghcVersion
  case packagesResult of
    Left err -> pure (Left (ProjectEnvironmentFailed err))
    Right (packageRoots, cabalFiles, packages) -> do
      providerInputsResult <- loadProviderDependencyInputs provider root
      pure do
        providerInputs <- providerInputsResult
        let localPackageNames = Set.fromList (map (.packageName) packages)
            declaredDependencies = Set.unions (concatMap (map (.dependencies) . (.components)) packages)
            requiredDependencies =
              declaredDependencies Set.\\ localPackageNames
            dependencySnapshot = dependencySnapshotForPackages packages
        pure
          PreparedProjectDescription
            { preparedPackageRoots = packageRoots,
              preparedCabalFiles = cabalFiles,
              preparedPackages = packages,
              preparedRequiredExternalDependencies = requiredDependencies,
              preparedConfigurationSnapshot =
                ProjectConfigurationSnapshot
                  { projectConfigurationProvider = provider,
                    projectConfigurationPackageRoots = packageRoots,
                    projectConfigurationDependencies = dependencySnapshot,
                    projectConfigurationProviderFiles = providerInputs
                  }
            }

dependencySnapshotForPackages :: [PackageData] -> Map.Map ComponentIdentity (Set.Set DependencyFingerprint)
dependencySnapshotForPackages packages =
  Map.fromList
    [ ( ComponentIdentity pkg.packageName component.componentName,
        component.dependencyRequirements
      )
    | pkg <- packages,
      component <- pkg.components
    ]

loadProviderDependencyInputs :: ProjectProvider -> FilePath -> IO (Either ProjectEnvironmentFailure [(FilePath, BS.ByteString)])
loadProviderDependencyInputs provider root = do
  let candidateFiles = providerDependencyInputPaths provider
  existing <- filterM (doesFileExist . (root </>)) candidateFiles
  fmap sequence $ forM existing \relativePath -> do
    let path = normalise (root </> relativePath)
    eiContent <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
    pure case eiContent of
      Left err -> Left (ProjectEnvironmentFailed ("Failed to read provider dependency input " <> path <> ": " <> show err))
      Right content -> Right (path, content)
