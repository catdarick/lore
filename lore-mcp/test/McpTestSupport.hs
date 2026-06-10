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
import Control.Monad (void, when)
import qualified Data.Aeson as J
import Data.Char (isSpace, toLower)
import Data.List (find, isPrefixOf, stripPrefix)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lore
  ( ProjectProvider (..),
    SessionConfig (..),
    defaultLoadHomeModulesOptions,
    loadHomeModules,
    loadStartupConfig,
    noLogHandle,
    renderSessionConfigError,
    startupSessionConfig,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), ToolWithoutArgs (..))
import Lore.Mcp.Monad (LoreMcpMonad, newLoreMcpContext, runLoreMcp)
import Lore.Tools.Render.Doc (ToLoreDoc (toLoreDoc))
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import System.Directory (copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, makeAbsolute, removeFile, removePathForcibly)
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
fixtureLoreMcpAtWithCache cacheEnabled fixtureRoot action = do
  provider <- resolveFixtureProjectProvider fixtureRoot
  withClearedGhcEnvironment do
    baseSessionConfig <-
      startupSessionConfig <$> (loadStartupConfig >>= either failWithSessionConfigError pure)
    context <- newLoreMcpContext cacheEnabled
    runLoreMcp (sessionConfig baseSessionConfig provider) context action
  where
    sessionConfig baseSessionConfig provider =
      baseSessionConfig
        { projectRoot = fixtureRoot,
          ghcWorkDir = fixtureRoot </> ".lore-work-test-mcp",
          configFilePath = fixtureRoot </> "lore.yaml",
          projectProviderOverride = Just provider,
          loggerHandle = noLogHandle,
          isTestSuiteFunctionalityRequired = True
        }

    failWithSessionConfigError =
      ioError . userError . T.unpack . renderSessionConfigError

withFixtureCopy :: (FilePath -> IO a) -> IO a
withFixtureCopy action = do
  fixtureRoot <- resolveFixtureRoot
  bracket (prepareFixtureCopy fixtureRoot) removePathForcibly action
  where
    prepareFixtureCopy fixtureRoot = do
      fixtureCopyRoot <- createTempDirectoryPath
      copyDirectoryRecursive fixtureRoot fixtureCopyRoot
      normalizeFixtureBuildFiles fixtureCopyRoot
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
  bracket
    ( do
        previousGhcEnvironment <- lookupEnv "GHC_ENVIRONMENT"
        previousGhcPackagePath <- lookupEnv "GHC_PACKAGE_PATH"
        unsetEnv "GHC_ENVIRONMENT"
        unsetEnv "GHC_PACKAGE_PATH"
        pure (previousGhcEnvironment, previousGhcPackagePath)
    )
    restore
    (const action)
  where
    restore (previousGhcEnvironment, previousGhcPackagePath) = do
      maybe (pure ()) (setEnv "GHC_ENVIRONMENT") previousGhcEnvironment
      maybe (pure ()) (setEnv "GHC_PACKAGE_PATH") previousGhcPackagePath

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

normalizeFixtureBuildFiles :: FilePath -> IO ()
normalizeFixtureBuildFiles fixtureCopyRoot = do
  provider <- detectFixtureBuildProvider
  removeBuildProviderFiles fixtureCopyRoot
  case provider of
    FixtureProviderCabal -> materializeCabalFixture fixtureCopyRoot
    FixtureProviderStack -> materializeStackFixture fixtureCopyRoot

data FixtureBuildProvider
  = FixtureProviderCabal
  | FixtureProviderStack

detectFixtureBuildProvider :: IO FixtureBuildProvider
detectFixtureBuildProvider = do
  maybeOverride <- lookupEnv "LORE_FIXTURE_PROVIDER"
  pure $ case fmap (map toLower) maybeOverride of
    Just "cabal" -> FixtureProviderCabal
    Just "stack" -> FixtureProviderStack
    _ -> FixtureProviderStack

removeBuildProviderFiles :: FilePath -> IO ()
removeBuildProviderFiles fixtureCopyRoot = do
  mapM_ removeFileIfExists buildProviderFiles
  where
    buildProviderFiles =
      [ fixtureCopyRoot </> "stack.yaml",
        fixtureCopyRoot </> "stack.yaml.lock",
        fixtureCopyRoot </> "cabal.project",
        fixtureCopyRoot </> "cabal.project.local",
        fixtureCopyRoot </> "cabal.project.freeze"
      ]

materializeCabalFixture :: FilePath -> IO ()
materializeCabalFixture fixtureCopyRoot = do
  writeFile (fixtureCopyRoot </> "cabal.project") "packages:\n  .\n"

materializeStackFixture :: FilePath -> IO ()
materializeStackFixture fixtureCopyRoot = do
  maybeProjectRoot <- findProjectRootWithStackFiles
  case maybeProjectRoot of
    Nothing -> error "Cannot materialize Stack fixture: project root with stack.yaml was not found."
    Just projectRoot -> do
      resolver <- readProjectResolver projectRoot
      writeFile
        (fixtureCopyRoot </> "stack.yaml")
        ("resolver: " <> resolver <> "\n\npackages:\n- .\n")
      copyProjectStackLockIfExists projectRoot fixtureCopyRoot

findProjectRootWithStackFiles :: IO (Maybe FilePath)
findProjectRootWithStackFiles = do
  cwd <- getCurrentDirectory
  let candidates = [cwd, cwd </> "..", cwd </> ".." </> "..", cwd </> ".." </> ".." </> ".."]
  go candidates
  where
    go [] = pure Nothing
    go (candidateRoot : restRoots) = do
      hasStackYaml <- doesFileExist (candidateRoot </> "stack.yaml")
      if hasStackYaml
        then pure (Just candidateRoot)
        else go restRoots

readProjectResolver :: FilePath -> IO String
readProjectResolver projectRoot = do
  stackYaml <- readFile (projectRoot </> "stack.yaml")
  case findResolver stackYaml of
    Just resolver -> pure resolver
    Nothing -> error "Cannot materialize Stack fixture: resolver was not found in project stack.yaml."
  where
    findResolver stackYamlContents =
      case find (isPrefixOf "resolver:" . dropWhile isSpace) (lines stackYamlContents) of
        Nothing -> Nothing
        Just resolverLine ->
          fmap (trim . dropWhile isSpace) (stripPrefix "resolver:" (dropWhile isSpace resolverLine))

copyProjectStackLockIfExists :: FilePath -> FilePath -> IO ()
copyProjectStackLockIfExists projectRoot fixtureCopyRoot = do
  let projectStackLockPath = projectRoot </> "stack.yaml.lock"
      fixtureStackLockPath = fixtureCopyRoot </> "stack.yaml.lock"
  projectHasStackLock <- doesFileExist projectStackLockPath
  when projectHasStackLock (copyFile projectStackLockPath fixtureStackLockPath)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)

resolveFixtureProjectProvider :: FilePath -> IO ProjectProvider
resolveFixtureProjectProvider fixtureRoot = do
  hasStackConfig <- doesFileExist (fixtureRoot </> "stack.yaml")
  pure $
    if hasStackConfig
      then StackProject
      else CabalProject
