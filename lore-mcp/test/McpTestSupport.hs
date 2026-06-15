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
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified GHC.Settings.Config as GHC.Settings
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
import System.Directory (copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import qualified System.Process as Process

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
  absoluteCandidates <- traverse makeAbsolute candidates
  maybeFixturePath <- findFirstExistingDirectory absoluteCandidates
  case maybeFixturePath of
    Just fixturePath -> pure fixturePath
    Nothing ->
      ioError . userError $
        "Cannot find lore-mcp test fixture. Searched:\n"
          <> unlines (map ("  - " <>) absoluteCandidates)

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
    SomeToolWithArgsStructured tool _ ->
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
    SomeToolWithoutArgsStructured tool _ ->
      error $
        "Tool "
          <> T.unpack tool.name
          <> " does not accept arguments."

callToolWithoutArgs :: SomeTool LoreMcpMonad -> LoreMcpMonad T.Text
callToolWithoutArgs someTool =
  case someTool of
    SomeToolWithoutArgs tool ->
      renderLoreDocMarkdown . toLoreDoc <$> tool.handler
    SomeToolWithoutArgsStructured tool _ ->
      renderLoreDocMarkdown . toLoreDoc <$> tool.handler
    SomeToolWithArgs tool ->
      error $
        "Tool "
          <> T.unpack tool.name
          <> " requires arguments."
    SomeToolWithArgsStructured tool _ ->
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

findFirstExistingDirectory :: [FilePath] -> IO (Maybe FilePath)
findFirstExistingDirectory candidatePaths =
  go candidatePaths
  where
    go [] = pure Nothing
    go (candidatePath : restPaths) = do
      exists <- doesDirectoryExist candidatePath
      if exists
        then pure (Just candidatePath)
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
  maybeStackExe <- lookupEnv "STACK_EXE"
  case fmap (map toLower) maybeOverride of
    Just "cabal" -> pure FixtureProviderCabal
    Just "stack" -> pure FixtureProviderStack
    Just unsupported ->
      error
        ( "Unsupported LORE_FIXTURE_PROVIDER value: "
            <> unsupported
            <> ". Expected \"stack\" or \"cabal\"."
        )
    Nothing
      | maybe False (not . null) maybeStackExe -> pure FixtureProviderStack
      | otherwise -> pure FixtureProviderCabal

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
  let packageDbRoot = fixtureCopyRoot </> "dist-newstyle" </> "packagedb"
      packageDb = packageDbRoot </> ("ghc-" <> GHC.Settings.cProjectVersion)
  createDirectoryIfMissing True packageDbRoot
  Process.callProcess "ghc-pkg" ["init", packageDb]

materializeStackFixture :: FilePath -> IO ()
materializeStackFixture fixtureCopyRoot =
  writeFile
    (fixtureCopyRoot </> "stack.yaml")
    ("resolver: ghc-" <> GHC.Settings.cProjectVersion <> "\n\npackages:\n- .\n")

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
