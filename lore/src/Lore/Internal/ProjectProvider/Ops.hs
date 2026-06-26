module Lore.Internal.ProjectProvider.Ops
  ( ProjectProviderOps (..),
    projectProviderOps,
    providerPackageRoots,
    providerMaterializeRunner,
    providerPrepareDependencies,
    providerRunInEnvironment,
    providerDependencyInputPaths,
  )
where

import Control.Exception (IOException, try)
import Lore.Internal.BuildTool.Command (boundedExcerpt, runProcessInWorkingDir, showCommand)
import Lore.Internal.Package.Discovery (discoverCabalPackageRoots, discoverStackPackageRoots)
import Lore.Internal.Package.Materialize (PackageMaterializeRunner (..), defaultPackageMaterializeRunner)
import Lore.Internal.Package.Root (PackageRoot)
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import System.Exit (ExitCode (..))
import System.Process (cwd, proc, readCreateProcessWithExitCode)

data ProjectProviderOps = ProjectProviderOps
  { projectProviderPackageRoots :: FilePath -> IO (Either String [PackageRoot]),
    projectProviderMaterializeRunner :: PackageMaterializeRunner,
    projectProviderPrepareDependencies :: FilePath -> IO (Either String ()),
    projectProviderRunInEnvironment :: FilePath -> String -> IO (Either String String),
    projectProviderDependencyInputPaths :: [FilePath]
  }

projectProviderOps :: ProjectProvider -> ProjectProviderOps
projectProviderOps = \case
  StackProject ->
    ProjectProviderOps
      { projectProviderPackageRoots = discoverStackPackageRoots,
        projectProviderMaterializeRunner =
          stackPackageMaterializeRunner,
        projectProviderPrepareDependencies =
          runDependencyPreparationCommand
            "stack"
            ["build", "--only-dependencies", "--test", "--bench", "--no-run-tests", "--no-run-benchmarks"],
        projectProviderRunInEnvironment = \projectRoot script ->
          runProcessInWorkingDir projectRoot "stack" ["exec", "--", "sh", "-lc", script],
        projectProviderDependencyInputPaths = ["stack.yaml", "stack.yaml.lock"]
      }
  CabalProject ->
    ProjectProviderOps
      { projectProviderPackageRoots = discoverCabalPackageRoots,
        projectProviderMaterializeRunner = defaultPackageMaterializeRunner,
        projectProviderPrepareDependencies =
          runDependencyPreparationCommand
            "cabal"
            ["build", "all", "--only-dependencies", "--enable-tests", "--enable-benchmarks"],
        projectProviderRunInEnvironment = \projectRoot script ->
          runProcessInWorkingDir projectRoot "cabal" ["exec", "--write-ghc-environment-files=never", "--", "sh", "-lc", script],
        projectProviderDependencyInputPaths = ["cabal.project", "cabal.project.local", "cabal.project.freeze"]
      }

providerPackageRoots :: ProjectProvider -> FilePath -> IO (Either String [PackageRoot])
providerPackageRoots provider =
  (projectProviderOps provider).projectProviderPackageRoots

providerMaterializeRunner :: ProjectProvider -> PackageMaterializeRunner
providerMaterializeRunner provider =
  (projectProviderOps provider).projectProviderMaterializeRunner

providerPrepareDependencies :: ProjectProvider -> FilePath -> IO (Either String ())
providerPrepareDependencies provider =
  (projectProviderOps provider).projectProviderPrepareDependencies

providerRunInEnvironment :: ProjectProvider -> FilePath -> String -> IO (Either String String)
providerRunInEnvironment provider =
  (projectProviderOps provider).projectProviderRunInEnvironment

providerDependencyInputPaths :: ProjectProvider -> [FilePath]
providerDependencyInputPaths provider =
  (projectProviderOps provider).projectProviderDependencyInputPaths

stackPackageMaterializeRunner :: PackageMaterializeRunner
stackPackageMaterializeRunner =
  PackageMaterializeRunner
    { runHpackGenerator = \projectRoot _packageRoot -> do
        hpackResult <- runProcessInWorkingDir projectRoot "stack" ["query"]
        pure (() <$ hpackResult)
    }

runDependencyPreparationCommand :: FilePath -> [String] -> FilePath -> IO (Either String ())
runDependencyPreparationCommand exe args projectRoot = do
  processResult <- try (readCreateProcessWithExitCode (proc exe args) {cwd = Just projectRoot} "") :: IO (Either IOException (ExitCode, String, String))
  pure case processResult of
    Left err ->
      Left $ "Failed to invoke dependency preparation command " <> showCommand exe args <> ": " <> show err
    Right (exitCode, stdoutText, stderrText) ->
      case exitCode of
        ExitSuccess -> Right ()
        ExitFailure code ->
          Left $
            "Dependency preparation command failed: "
              <> showCommand exe args
              <> "\nExit code: "
              <> show code
              <> "\nStdout:\n"
              <> boundedExcerpt stdoutText
              <> "\nStderr:\n"
              <> boundedExcerpt stderrText
