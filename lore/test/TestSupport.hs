module TestSupport
  ( FixtureBuildProvider (..),
    FixtureContext,
    fixtureSourceRoot,
    fixtureProjectRoot,
    selectFixtureBuildProvider,
    withFixtureContext,
    withFixtureSpec,
    fixtureLore,
    fixtureLoreAt,
    fixtureLoreAtWithConfig,
    withFixtureCopy,
    findSymbols,
    findRootSymbols,
    lookupRootSymbolInfo,
    lookupRootSymbolChains,
    listExportedSymbolsByModule,
    filterExportedSymbolNodesByTypeHint,
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Char (isSpace, toLower)
import Data.List (find, isInfixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified GHC.Plugins as GHC
import qualified GHC.Settings.Config as GHC.Settings
import qualified Lore
import Lore.Logger (LoggerHandle, noLogHandle)
import Lore.Monad (LoreMonadT)
import Lore.Session (SessionConfig (..), defaultSessionConfig, runLore)
import qualified Lore.Session as Session
import System.Directory (copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import qualified System.Process as Process
import Test.Hspec (Spec, SpecWith, around)

data FixtureBuildProvider
  = FixtureProviderCabal
  | FixtureProviderStack
  deriving (Eq, Show)

data FixtureContext = FixtureContext
  { fixtureSourceRoot :: FilePath,
    fixtureProjectRoot :: FilePath,
    fixtureBuildProvider :: FixtureBuildProvider,
    fixtureProjectProvider :: Session.ProjectProvider
  }

withFixtureSpec :: SpecWith FixtureContext -> Spec
withFixtureSpec =
  around withFixtureContext

withFixtureContext :: (FixtureContext -> IO a) -> IO a
withFixtureContext =
  bracket prepareFixtureContext cleanupFixtureContext

prepareFixtureContext :: IO FixtureContext
prepareFixtureContext = do
  sourceRoot <- makeAbsolute ("test" </> "fixtures" </> "demo")
  provider <- detectFixtureBuildProvider
  projectRoot <- createTempDirectoryPath
  copyFixtureSourceTree sourceRoot projectRoot
  materializeFixture provider projectRoot
  pure
    FixtureContext
      { fixtureSourceRoot = sourceRoot,
        fixtureProjectRoot = projectRoot,
        fixtureBuildProvider = provider,
        fixtureProjectProvider = toProjectProvider provider
      }

cleanupFixtureContext :: FixtureContext -> IO ()
cleanupFixtureContext context =
  removePathForcibly context.fixtureProjectRoot

fixtureLore :: FixtureContext -> LoreMonadT IO a -> IO a
fixtureLore context action =
  fixtureLoreAt context context.fixtureProjectRoot action

fixtureLoreAt :: FixtureContext -> FilePath -> LoreMonadT IO a -> IO a
fixtureLoreAt context fixtureRoot action =
  fixtureLoreAtWithConfig
    context
    (sessionConfigWithLogger noLogHandle)
    fixtureRoot
    action

fixtureLoreAtWithConfig :: FixtureContext -> SessionConfig -> FilePath -> LoreMonadT IO a -> IO a
fixtureLoreAtWithConfig context sessionConfig fixtureRoot action =
  withClearedGhcEnvironment $
    runLore
      sessionConfig
        { Session.projectRoot = fixtureRoot,
          Session.ghcWorkDir = fixtureRoot </> ".lore-work-test",
          Session.configFilePath = fixtureRoot </> "lore.yaml",
          Session.projectProviderOverride = Just context.fixtureProjectProvider
        }
      action

sessionConfigWithLogger :: LoggerHandle -> SessionConfig
sessionConfigWithLogger handle =
  defaultSessionConfig
    { loggerHandle = handle
    }

withFixtureCopy :: FixtureContext -> (FilePath -> IO a) -> IO a
withFixtureCopy context =
  bracket prepareFixtureCopy removePathForcibly
  where
    prepareFixtureCopy = do
      fixtureCopyRoot <- createTempDirectoryPath
      copyFixtureSourceTree context.fixtureSourceRoot fixtureCopyRoot
      materializeFixture context.fixtureBuildProvider fixtureCopyRoot
      pure fixtureCopyRoot

createTempDirectoryPath :: IO FilePath
createTempDirectoryPath = do
  timestamp <- round . (* 1_000_000) <$> getPOSIXTime
  (tempFilePath, handle) <- openTempFile "/tmp" ("lore-fixture-" <> show (timestamp :: Integer))
  hClose handle
  removeFile tempFilePath
  createDirectory tempFilePath
  pure tempFilePath

copyFixtureSourceTree :: FilePath -> FilePath -> IO ()
copyFixtureSourceTree sourceDir targetDir = do
  createDirectoryIfMissing True targetDir
  entries <- listDirectory sourceDir
  mapM_ copyEntry (filter (`Set.notMember` fixtureGeneratedEntryNames) entries)
  where
    copyEntry entryName = do
      let sourcePath = sourceDir </> entryName
          targetPath = targetDir </> entryName
      isDirectory <- doesDirectoryExist sourcePath
      if isDirectory
        then copyFixtureSourceTree sourcePath targetPath
        else copyFile sourcePath targetPath

fixtureGeneratedEntryNames :: Set.Set FilePath
fixtureGeneratedEntryNames =
  Set.fromList
    [ ".lore-work-test",
      ".stack-work",
      "dist-newstyle",
      "stack.yaml",
      "stack.yaml.lock",
      "cabal.project",
      "cabal.project.local",
      "cabal.project.freeze",
      ".hspec-failures"
    ]

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

materializeFixture :: FixtureBuildProvider -> FilePath -> IO ()
materializeFixture provider fixtureCopyRoot = do
  case provider of
    FixtureProviderCabal -> materializeCabalFixture fixtureCopyRoot
    FixtureProviderStack -> materializeStackFixture fixtureCopyRoot

detectFixtureBuildProvider :: IO FixtureBuildProvider
detectFixtureBuildProvider = do
  override <- lookupEnv "LORE_FIXTURE_PROVIDER"
  maybeStackExe <- lookupEnv "STACK_EXE"
  maybeGhcEnvironment <- lookupEnv "GHC_ENVIRONMENT"
  maybeGhcPackagePath <- lookupEnv "GHC_PACKAGE_PATH"

  either error pure $
    selectFixtureBuildProvider
      override
      maybeStackExe
      maybeGhcEnvironment
      maybeGhcPackagePath

selectFixtureBuildProvider ::
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Either String FixtureBuildProvider
selectFixtureBuildProvider maybeOverride maybeStackExe maybeGhcEnvironment maybeGhcPackagePath =
  case fmap (map toLower) maybeOverride of
    Just "stack" -> Right FixtureProviderStack
    Just "cabal" -> Right FixtureProviderCabal
    Just unsupported ->
      Left
        ( "Unsupported LORE_FIXTURE_PROVIDER value: "
            <> unsupported
            <> ". Expected \"stack\" or \"cabal\"."
        )
    Nothing
      | isNonEmpty maybeStackExe ->
          Right FixtureProviderStack
      | maybeGhcEnvironment == Just "-",
        isNonEmpty maybeGhcPackagePath ->
          Right FixtureProviderStack
      | isNonEmpty maybeGhcEnvironment ->
          Right FixtureProviderCabal
      | otherwise ->
          Left
            "Could not detect the fixture build provider. Set LORE_FIXTURE_PROVIDER to \"stack\" or \"cabal\"."
  where
    isNonEmpty =
      maybe False (not . null)

toProjectProvider :: FixtureBuildProvider -> Session.ProjectProvider
toProjectProvider FixtureProviderStack = Session.StackProject
toProjectProvider FixtureProviderCabal = Session.CabalProject

materializeCabalFixture :: FilePath -> IO ()
materializeCabalFixture fixtureCopyRoot = do
  writeFile (fixtureCopyRoot </> "cabal.project") "packages:\n  .\n"
  prepareCabalFixturePackageEnvironment fixtureCopyRoot

prepareCabalFixturePackageEnvironment :: FilePath -> IO ()
prepareCabalFixturePackageEnvironment fixtureCopyRoot = do
  withClearedGhcEnvironment $
    do
      runFixtureCommand
        fixtureCopyRoot
        "cabal"
        ["build", "--only-dependencies", "all"]
      ensureCabalFixturePackageDb fixtureCopyRoot

ensureCabalFixturePackageDb :: FilePath -> IO ()
ensureCabalFixturePackageDb fixtureCopyRoot = do
  ghcVersion <- trim <$> readFixtureCommand fixtureCopyRoot "ghc" ["--numeric-version"]
  let packageDb = fixtureCopyRoot </> "dist-newstyle" </> "packagedb" </> ("ghc-" <> ghcVersion)
  packageDbExists <- doesDirectoryExist packageDb
  when (not packageDbExists) do
    createDirectoryIfMissing True (fixtureCopyRoot </> "dist-newstyle" </> "packagedb")
    runFixtureCommand fixtureCopyRoot "ghc-pkg" ["init", packageDb]

materializeStackFixture :: FilePath -> IO ()
materializeStackFixture fixtureCopyRoot =
  writeFile
    (fixtureCopyRoot </> "stack.yaml")
    ("resolver: ghc-" <> GHC.Settings.cProjectVersion <> "\n\npackages:\n- .\n")

runFixtureCommand :: FilePath -> FilePath -> [String] -> IO ()
runFixtureCommand cwd command args = do
  _ <- readFixtureCommandWithExit cwd command args
  pure ()

readFixtureCommand :: FilePath -> FilePath -> [String] -> IO String
readFixtureCommand cwd command args = do
  (stdoutText, _) <- readFixtureCommandWithExit cwd command args
  pure stdoutText

readFixtureCommandWithExit :: FilePath -> FilePath -> [String] -> IO (String, String)
readFixtureCommandWithExit cwd command args = do
  (exitCode, stdoutText, stderrText) <-
    Process.readCreateProcessWithExitCode
      (Process.proc command args) {Process.cwd = Just cwd}
      ""
  case exitCode of
    ExitSuccess -> pure (stdoutText, stderrText)
    ExitFailure code ->
      error $
        unlines
          [ "Fixture preparation command failed.",
            "cwd: " <> cwd,
            "command: " <> unwords (command : args),
            "exit code: " <> show code,
            "stdout:",
            stdoutText,
            "stderr:",
            stderrText
          ]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

findSymbols :: (Lore.MonadLore m) => Text -> m [Lore.Symbol]
findSymbols query =
  Set.toList <$> Lore.findMatchingSymbols (Lore.parseAndNormalizeName query)

findRootSymbols :: (Lore.MonadLore m) => Text -> m [Lore.Symbol]
findRootSymbols query = do
  symbols <- findSymbols query
  pathsToRoot <- mapM (Lore.resolvePathToRoot . (.name)) symbols
  preferredRootNames <- pickPreferredRootNames (map (NE.last . (.unPathToRoot)) pathsToRoot)
  pure (catMaybes (map (`findSymbolByName` symbols) preferredRootNames))
  where
    findSymbolByName targetName =
      find ((== targetName) . (.name))

    pickPreferredRootNames rootNames =
      concat <$> mapM pickPreferredByOccName (Map.elems groupedByOccName)
      where
        groupedByOccName =
          Map.fromListWith
            (<>)
            [ (Lore.occName (Lore.parseAndNormalizeName (T.pack (GHC.getOccString name))), [name])
            | name <- rootNames
            ]

    pickPreferredByOccName [] =
      pure []
    pickPreferredByOccName namesForOcc = do
      categorizedNames <- mapM classify namesForOcc
      let nonValueNames =
            [ name
            | (name, category) <- categorizedNames,
              category /= Lore.SymbolValue
            ]
      pure $
        case nonValueNames of
          preferredName : _ -> [preferredName]
          [] -> take 1 namesForOcc

    classify name = do
      maybeInfo <- Lore.lookupSymbolInfo name
      pure (name, maybe Lore.SymbolUnknown (Lore.classifySymbolCategory . Lore.symbolThing) maybeInfo)

lookupRootSymbolInfo :: (Lore.MonadLore m) => Text -> m [Lore.SymbolInfo]
lookupRootSymbolInfo query = do
  rootSymbols <- findRootSymbols query
  catMaybes <$> mapM (Lore.lookupSymbolInfo . (.name)) rootSymbols

lookupRootSymbolChains :: (Lore.MonadLore m) => Text -> m [[GHC.Name]]
lookupRootSymbolChains query = do
  symbols <- findSymbols query
  pathsToRoot <- mapM (Lore.resolvePathToRoot . (.name)) symbols
  let mergedPaths = mergePathsToRootOn renderName pathsToRoot
  pure (map (NE.toList . (.unPathToRoot)) mergedPaths)
  where
    renderName name =
      case GHC.nameModule_maybe name of
        Nothing ->
          "<no-module>." <> GHC.getOccString name
        Just module_ ->
          GHC.moduleNameString (GHC.moduleName module_) <> "." <> GHC.getOccString name

mergePathsToRootOn :: (Ord b) => (a -> b) -> [Lore.PathToRoot a] -> [Lore.PathToRoot a]
mergePathsToRootOn getKey paths =
  Map.elems (Map.fromListWith mergePaths pathPairs)
  where
    pathPairs =
      [ (getKey (NE.last path.unPathToRoot), path)
      | path <- paths
      ]

    mergePaths path1 path2 =
      let values1 = NE.toList path1.unPathToRoot
          values2 = NE.toList path2.unPathToRoot
          keys1 = map getKey values1
          keys2 = map getKey values2
       in if
            | keys1 `isInfixOf` keys2 -> path2
            | keys2 `isInfixOf` keys1 -> path1
            | otherwise ->
                let (primaryValues, secondaryValues) =
                      if length values1 >= length values2
                        then (values1, values2)
                        else (values2, values1)
                    primaryKeys = map getKey primaryValues
                    secondaryUniquePart = filter (\value -> getKey value `notElem` primaryKeys) secondaryValues
                 in Lore.PathToRoot (NE.fromList (secondaryUniquePart <> primaryValues))

listExportedSymbolsByModule :: (Lore.MonadLore m) => Text -> Maybe Text -> m [Lore.ExportedSymbolNode]
listExportedSymbolsByModule moduleName maybePackageName = do
  let normalizedModuleName =
        Lore.mkNormalizedModuleName moduleName
  maybeModule <- Lore.resolveModule normalizedModuleName maybePackageName
  maybe (pure []) Lore.listSymbolsExportedByModule maybeModule

filterExportedSymbolNodesByTypeHint :: Text -> [Lore.ExportedSymbolNode] -> [Lore.ExportedSymbolNode]
filterExportedSymbolNodesByTypeHint typeHint =
  Lore.filterExportedSymbolNodesByTypeHint (Lore.occName (Lore.parseAndNormalizeName typeHint))
