module Lore.Internal.BuildTool.Command
  ( runProcessInWorkingDir,
    showCommand,
    boundedExcerpt,
  )
where

import Control.Exception (IOException, handle)
import System.Exit (ExitCode (..))
import System.Process (cwd, proc, readCreateProcessWithExitCode)

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

showCommand :: FilePath -> [String] -> String
showCommand exe args = unwords (map show (exe : args))

boundedExcerpt :: String -> String
boundedExcerpt text =
  let limit = 4000
   in if length text <= limit
        then text
        else take limit text <> "\n... <truncated>"
