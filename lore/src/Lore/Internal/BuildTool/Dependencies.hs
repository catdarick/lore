module Lore.Internal.BuildTool.Dependencies
  ( prepareProjectDependencies,
  )
where

import Control.Exception (IOException, try)
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

prepareProjectDependencies :: ProjectProvider -> FilePath -> IO (Either String ())
prepareProjectDependencies provider projectRoot = do
  let (exe, args) = dependencyPreparationCommand provider
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

dependencyPreparationCommand :: ProjectProvider -> (FilePath, [String])
dependencyPreparationCommand provider =
  case provider of
    StackProject ->
      ("stack", ["build", "--only-dependencies", "--test", "--bench", "--no-run-tests", "--no-run-benchmarks"])
    CabalProject ->
      ("cabal", ["build", "all", "--only-dependencies", "--enable-tests", "--enable-benchmarks"])

showCommand :: FilePath -> [String] -> String
showCommand exe args = unwords (map show (exe : args))

boundedExcerpt :: String -> String
boundedExcerpt text =
  let limit = 4000
   in if length text <= limit
        then text
        else take limit text <> "\n... <truncated>"
