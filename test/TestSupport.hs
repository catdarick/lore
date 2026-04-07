module TestSupport (fixtureLore, fixtureLoreAt, fixtureLoreAtWithLogger, withFixtureCopy) where

import Control.Exception (bracket)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Internal.Logger (LoggerHandle, noLogHandle)
import Lore (runLoreMonadT)
import Monad (LoreMonadT)
import Session (defaultSessionConfig)
import qualified Session
import System.Directory (copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

fixtureLore :: LoreMonadT IO a -> IO a
fixtureLore action = do
  fixtureRoot <- makeAbsolute ("test" </> "fixtures" </> "demo")
  fixtureLoreAt fixtureRoot action

fixtureLoreAt :: FilePath -> LoreMonadT IO a -> IO a
fixtureLoreAt fixtureRoot action =
  fixtureLoreAtWithLogger noLogHandle fixtureRoot action

fixtureLoreAtWithLogger :: LoggerHandle -> FilePath -> LoreMonadT IO a -> IO a
fixtureLoreAtWithLogger loggerHandle fixtureRoot action =
  withClearedGhcEnvironment $
    runLoreMonadT
      defaultSessionConfig
        { Session.projectRoot = fixtureRoot,
          Session.ghcWorkDir = fixtureRoot </> ".lore-work-test",
          Session.loggerHandle = loggerHandle
        }
      action

withFixtureCopy :: (FilePath -> IO a) -> IO a
withFixtureCopy action = do
  fixtureRoot <- makeAbsolute ("test" </> "fixtures" </> "demo")
  bracket (prepareFixtureCopy fixtureRoot) removePathForcibly action
  where
    prepareFixtureCopy fixtureRoot = do
      fixtureCopyRoot <- createTempDirectoryPath
      copyDirectoryRecursive fixtureRoot fixtureCopyRoot
      pure fixtureCopyRoot

createTempDirectoryPath :: IO FilePath
createTempDirectoryPath = do
  timestamp <- round . (* 1_000_000) <$> getPOSIXTime
  (tempFilePath, handle) <- openTempFile "/tmp" ("lore-fixture-" <> show (timestamp :: Integer))
  hClose handle
  removeFile tempFilePath
  createDirectory tempFilePath
  pure tempFilePath

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive sourceDir targetDir = do
  createDirectoryIfMissing True targetDir
  entries <- listDirectory sourceDir
  mapM_ copyEntry entries
  where
    copyEntry entryName = do
      let sourcePath = sourceDir </> entryName
          targetPath = targetDir </> entryName
      isDirectory <- doesDirectoryExist sourcePath
      if isDirectory
        then copyDirectoryRecursive sourcePath targetPath
        else copyFile sourcePath targetPath

withClearedGhcEnvironment :: IO a -> IO a
withClearedGhcEnvironment action =
  bracket (lookupEnv "GHC_ENVIRONMENT" <* unsetEnv "GHC_ENVIRONMENT") restore (const action)
  where
    restore =
      maybe (pure ()) (setEnv "GHC_ENVIRONMENT")
