module Lore.Internal.BuildTool.Environment
  ( runInBuildToolEnvironment,
    runProcessInWorkingDir,
  )
where

import Control.Exception (IOException, handle)
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

runInBuildToolEnvironment :: ProjectProvider -> FilePath -> String -> IO (Either String String)
runInBuildToolEnvironment provider projectRoot script =
  case provider of
    CabalProject ->
      runProcessInWorkingDir
        projectRoot
        "cabal"
        ["exec", "--write-ghc-environment-files=never", "--", "sh", "-lc", script]
    StackProject ->
      runProcessInWorkingDir
        projectRoot
        "stack"
        ["exec", "--", "sh", "-lc", script]

runProcessInWorkingDir :: FilePath -> FilePath -> [String] -> IO (Either String String)
runProcessInWorkingDir workingDir command arguments =
  handle (pure . Left . showAsIoException) do
    (exitCode, stdoutText, stderrText) <-
      readCreateProcessWithExitCode (proc command arguments) {cwd = Just workingDir} ""
    pure $ case exitCode of
      ExitSuccess -> Right stdoutText
      ExitFailure code ->
        Left
          ( "Command '"
              <> command
              <> " "
              <> unwords arguments
              <> "' failed with exit code "
              <> show code
              <> ". Stdout: "
              <> stdoutText
              <> ". Stderr: "
              <> stderrText
          )
  where
    showAsIoException :: IOException -> String
    showAsIoException = show
