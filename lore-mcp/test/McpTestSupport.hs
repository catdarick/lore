module McpTestSupport
  ( fixtureLoreMcp,
    fixtureLoreMcpWithCache,
    fixtureLoreMcpAtWithCache,
    withFixtureCopy,
    loadFixtureHomeModules,
    callToolWithArgs,
    callToolWithoutArgs,
  )
where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.Aeson as J
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lore
  ( SessionConfig (..),
    defaultLoadHomeModulesOptions,
    defaultSessionConfig,
    loadHomeModules,
    noLogHandle,
  )
import Lore.Mcp.Internal.LoreDoc (ToLoreDoc (toLoreDoc))
import Lore.Mcp.Internal.LoreDoc.Markdown (renderLoreDocMarkdown)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), ToolWithoutArgs (..))
import Lore.Mcp.Monad (LoreMcpMonad, newLoreMcpContext, runLoreMcp)
import System.Directory (copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

fixtureLoreMcp :: LoreMcpMonad a -> IO a
fixtureLoreMcp =
  fixtureLoreMcpWithCache False

fixtureLoreMcpWithCache :: Bool -> LoreMcpMonad a -> IO a
fixtureLoreMcpWithCache cacheEnabled action =
  withFixtureCopy \fixtureRoot ->
    fixtureLoreMcpAtWithCache cacheEnabled fixtureRoot action

fixtureLoreMcpAtWithCache :: Bool -> FilePath -> LoreMcpMonad a -> IO a
fixtureLoreMcpAtWithCache cacheEnabled fixtureRoot action =
  withClearedGhcEnvironment do
    context <- newLoreMcpContext cacheEnabled
    runLoreMcp sessionConfig context action
  where
    sessionConfig =
      defaultSessionConfig
        { projectRoot = fixtureRoot,
          ghcWorkDir = fixtureRoot </> ".lore-work-test-mcp",
          loggerHandle = noLogHandle,
          isTestSuiteFunctionalityRequired = True
        }

withFixtureCopy :: (FilePath -> IO a) -> IO a
withFixtureCopy action = do
  fixtureRoot <- resolveFixtureRoot
  bracket (prepareFixtureCopy fixtureRoot) removePathForcibly action
  where
    prepareFixtureCopy fixtureRoot = do
      fixtureCopyRoot <- createTempDirectoryPath
      copyDirectoryRecursive fixtureRoot fixtureCopyRoot
      pure fixtureCopyRoot

resolveFixtureRoot :: IO FilePath
resolveFixtureRoot = do
  let candidates =
        [ "test" </> "fixtures" </> "demo",
          "lore-mcp" </> "test" </> "fixtures" </> "demo"
        ]
  maybeFixturePath <- findFirstExistingAbsolutePath candidates
  case maybeFixturePath of
    Just fixturePath -> pure fixturePath
    Nothing -> makeAbsolute (head candidates)

loadFixtureHomeModules :: LoreMcpMonad ()
loadFixtureHomeModules =
  void (loadHomeModules defaultLoadHomeModulesOptions)

callToolWithArgs :: SomeTool LoreMcpMonad -> J.Value -> LoreMcpMonad T.Text
callToolWithArgs someTool args =
  case someTool of
    SomeToolWithArgs tool ->
      case J.fromJSON args of
        J.Error errorMessage ->
          error $
            "Failed to parse arguments for tool "
              <> T.unpack tool.name
              <> ": "
              <> errorMessage
        J.Success parsedArgs ->
          renderLoreDocMarkdown . toLoreDoc <$> tool.handler parsedArgs
    SomeToolWithoutArgs tool ->
      error $
        "Tool "
          <> T.unpack tool.name
          <> " does not accept arguments."

callToolWithoutArgs :: SomeTool LoreMcpMonad -> LoreMcpMonad T.Text
callToolWithoutArgs someTool =
  case someTool of
    SomeToolWithoutArgs tool ->
      renderLoreDocMarkdown . toLoreDoc <$> tool.handler
    SomeToolWithArgs tool ->
      error $
        "Tool "
          <> T.unpack tool.name
          <> " requires arguments."

withClearedGhcEnvironment :: IO a -> IO a
withClearedGhcEnvironment action =
  bracket (lookupEnv "GHC_ENVIRONMENT" <* unsetEnv "GHC_ENVIRONMENT") restore (const action)
  where
    restore =
      maybe (pure ()) (setEnv "GHC_ENVIRONMENT")

createTempDirectoryPath :: IO FilePath
createTempDirectoryPath = do
  timestamp <- round . (* 1_000_000) <$> getPOSIXTime
  (tempFilePath, handle) <- openTempFile "/tmp" ("lore-mcp-fixture-" <> show (timestamp :: Integer))
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

findFirstExistingAbsolutePath :: [FilePath] -> IO (Maybe FilePath)
findFirstExistingAbsolutePath candidatePaths =
  go candidatePaths
  where
    go [] = pure Nothing
    go (candidatePath : restPaths) = do
      absolutePath <- makeAbsolute candidatePath
      exists <- doesDirectoryExist absolutePath
      if exists
        then pure (Just absolutePath)
        else go restPaths
