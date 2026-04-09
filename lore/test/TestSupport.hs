module TestSupport (fixtureLore, fixtureLoreAt, fixtureLoreAtWithConfig, fixtureLoreAtWithLogger, withFixtureCopy) where

import Control.Exception (bracket)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lore.Logger (LoggerHandle, noLogHandle)
import Lore.Monad (LoreMonadT)
import Lore.Session (SessionConfig, defaultSessionConfig, runLore)
import qualified Lore.Session as Session
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
  fixtureLoreAtWithConfig
    (sessionConfigWithLogger noLogHandle)
    fixtureRoot
    action

fixtureLoreAtWithLogger :: LoggerHandle -> FilePath -> LoreMonadT IO a -> IO a
fixtureLoreAtWithLogger loggerHandle fixtureRoot action =
  fixtureLoreAtWithConfig
    (sessionConfigWithLogger loggerHandle)
    fixtureRoot
    action

fixtureLoreAtWithConfig :: SessionConfig -> FilePath -> LoreMonadT IO a -> IO a
fixtureLoreAtWithConfig sessionConfig fixtureRoot action =
  withClearedGhcEnvironment $
    runLore
      sessionConfig
        { Session.projectRoot = fixtureRoot,
          Session.ghcWorkDir = fixtureRoot </> ".lore-work-test"
        }
      action

sessionConfigWithLogger :: LoggerHandle -> SessionConfig
sessionConfigWithLogger loggerHandle =
  Session.SessionConfig
    { Session.projectRoot = projectRoot,
      Session.ghcWorkDir = ghcWorkDir,
      Session.loggerHandle = loggerHandle,
      Session.customPrelude = customPrelude,
      Session.parallelWorkersLimit = parallelWorkersLimit
    }
  where
    Session.SessionConfig
      { Session.projectRoot,
        Session.ghcWorkDir,
        Session.customPrelude,
        Session.parallelWorkersLimit
      } = defaultSessionConfig

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
