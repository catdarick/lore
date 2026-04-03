module Internal.PackageDB where

import Control.Exception (IOException, handle)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath (normalise, splitSearchPath, (</>))
import System.Process

-- TODO: add support of Cabal
resolvePackageDbPaths :: FilePath -> IO (Either String [FilePath])
resolvePackageDbPaths projectRoot = do
  stackConfigExists <- doesFileExist (projectRoot </> "stack.yaml")
  if stackConfigExists
    then do
      res <- runProcess' "stack" ["path", "--ghc-package-path"]
      case res of
        Left err -> pure $ Left $ "Failed to resolve package database paths: " <> err
        Right output -> pure $ Right $ normalizeSearchPathOutput output
    else do
      pure $ Left "No stack.yaml file found. Only Stack projects are supported at the moment."
  where
    runProcess' :: FilePath -> [String] -> IO (Either String String)
    runProcess' command arguments =
      handle (\err -> pure (Left (show (err :: IOException)))) do
        (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (proc command arguments) {cwd = Just projectRoot} ""
        case exitCode of
          ExitSuccess -> pure (Right stdoutText)
          ExitFailure code -> do
            let msg = "Command '" <> command <> " " <> unwords arguments <> "' failed with exit code " <> show code <> ". Stderr: " <> stderrText
            pure (Left msg)

    normalizeSearchPathOutput :: String -> [FilePath]
    normalizeSearchPathOutput =
      map normalise
        . filter (not . null)
        . splitSearchPath
        . trim

    trim :: String -> String
    trim = reverse . dropWhile isTrimChar . reverse . dropWhile isTrimChar
      where
        isTrimChar ch = ch `elem` ['\n', '\r', ' ', '\t']
