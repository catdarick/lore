module DeadCodeSpec (spec) where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Plugins as GHC
import Lore
  ( DeadCodeOptions (..),
    DeadCodeResult (..),
    DeadDefinition (..),
    Diagnostic (..),
    LoadHomeModulesResult (..),
    MonadLore,
    Symbol (..),
    defaultLoadHomeModulesOptions,
    findDeadCode,
    loadHomeModules,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (findSymbols, fixtureLoreAt, withFixtureCopy)

spec :: Spec
spec = do
  describe "findDeadCode" do
    it "keeps transitive dependencies reachable from executable main alive" do
      withFixtureDeadCodeProject \fixtureRoot -> do
        result <-
          runFindDeadCode fixtureRoot defaultDeadCodeOptions
        let deadNames = deadDefinitionOccNames result
        deadNames `shouldContain` ["deadRoot", "deadDependency"]
        deadNames `shouldNotContain` ["liveRoot", "liveDependency"]
        deadNames `shouldNotContain` ["ToSchema", "schema", "IsFieldMetadata", "modifySchema", "IsExample", "exampleToJSON"]
        deadNames `shouldNotContainPrefix` "$fToSchema"
        deadNames `shouldNotContainPrefix` "$fIsFieldMetadata"
        deadNames `shouldNotContainPrefix` "$fIsExample"
        deadNames `shouldNotContain` ["testMainHelper"]

    it "filters reported dead definitions by target module without changing global reachability" do
      withFixtureDeadCodeProject \fixtureRoot -> do
        result <-
          fixtureLoreAt fixtureRoot do
            requireLoadedHomeModules
            targetModule <- requireSymbolModule "DeadCode.Lib" "deadRoot"
            findDeadCode defaultDeadCodeOptions {deadCodeTargetModules = Just (Set.singleton targetModule)}

        let deadNames = deadDefinitionOccNames result
        deadNames `shouldContain` ["deadRoot", "deadDependency"]
        deadNames `shouldNotContain` ["liveRoot", "liveDependency"]
        result.deadCodeTotalDefinitions `shouldSatisfy` (> 0)

    it "treats alive modules as root sets" do
      withFixtureDeadCodeProject \fixtureRoot -> do
        result <-
          fixtureLoreAt fixtureRoot do
            requireLoadedHomeModules
            aliveModule <- requireSymbolModule "Dev" "runSomething"
            findDeadCode defaultDeadCodeOptions {deadCodeAliveModules = Set.singleton aliveModule}

        let deadNames = deadDefinitionOccNames result
        deadNames `shouldNotContain` ["deadRoot", "deadDependency"]

    it "treats alive symbols as roots" do
      withFixtureDeadCodeProject \fixtureRoot -> do
        result <-
          fixtureLoreAt fixtureRoot do
            requireLoadedHomeModules
            deadRoot <- requireSymbol "DeadCode.Lib" "deadRoot"
            findDeadCode defaultDeadCodeOptions {deadCodeAliveNames = Set.singleton deadRoot}

        let deadNames = deadDefinitionOccNames result
        deadNames `shouldNotContain` ["deadRoot", "deadDependency"]

    it "does not treat test mains as alive roots" do
      withFixtureDeadCodeProject \fixtureRoot -> do
        result <-
          runFindDeadCode fixtureRoot defaultDeadCodeOptions
        deadDefinitionOccNames result `shouldContain` ["testOnly"]

    it "reports entry-module resolution warnings instead of silently dropping them" do
      withFixtureDeadCodeProject \fixtureRoot -> do
        TIO.appendFile (fixtureRoot </> "package.yaml") brokenEntryPackageSuffix
        result <-
          runFindDeadCode fixtureRoot defaultDeadCodeOptions
        map T.unpack result.deadCodeWarnings
          `shouldSatisfy` any (List.isInfixOf "Failed to resolve entry module for")

defaultDeadCodeOptions :: DeadCodeOptions
defaultDeadCodeOptions =
  DeadCodeOptions
    { deadCodeTargetModules = Nothing,
      deadCodeAliveModules = Set.empty,
      deadCodeAliveNames = Set.empty
    }

runFindDeadCode :: FilePath -> DeadCodeOptions -> IO DeadCodeResult
runFindDeadCode fixtureRoot options =
  fixtureLoreAt fixtureRoot do
    requireLoadedHomeModules
    findDeadCode options

requireSymbol :: (MonadLore m) => String -> String -> m GHC.Name
requireSymbol moduleName occName = do
  symbols <- findSymbols (T.pack (moduleName <> "." <> occName))
  case List.find (matchesSymbol moduleName occName) symbols of
    Just symbol ->
      pure symbol.name
    Nothing ->
      error ("failed to resolve symbol: " <> moduleName <> "." <> occName)

requireSymbolModule :: (MonadLore m) => String -> String -> m GHC.Module
requireSymbolModule moduleName occName = do
  symbolName <- requireSymbol moduleName occName
  case GHC.nameModule_maybe symbolName of
    Just module_ ->
      pure module_
    Nothing ->
      error ("resolved symbol has no module: " <> moduleName <> "." <> occName)

requireLoadedHomeModules :: (MonadLore m) => m ()
requireLoadedHomeModules = do
  loadResult <- loadHomeModules defaultLoadHomeModulesOptions
  if loadResult.loadHomeModulesSucceeded
    then pure ()
    else error (renderLoadFailure loadResult)

renderLoadFailure :: LoadHomeModulesResult -> String
renderLoadFailure loadResult =
  unlines $
    [ "loadHomeModules failed in dead-code fixture",
      "loaded=" <> show loadResult.loadHomeModulesLoaded,
      "failed=" <> show loadResult.loadHomeModulesFailed,
      "total=" <> show loadResult.loadHomeModulesTotal
    ]
      <> map (T.unpack . (.diagnosticMessage)) loadResult.loadHomeModulesDiagnostics

matchesSymbol :: String -> String -> Symbol -> Bool
matchesSymbol moduleName occName symbol =
  case GHC.nameModule_maybe symbol.name of
    Nothing ->
      False
    Just module_ ->
      GHC.moduleNameString (GHC.moduleName module_) == moduleName
        && GHC.getOccString symbol.name == occName

deadDefinitionOccNames :: DeadCodeResult -> [String]
deadDefinitionOccNames result =
  [ GHC.getOccString name
  | deadDefinition <- result.deadCodeDeadDefinitions,
    name <- Set.toList deadDefinition.deadDefinitionNames
  ]

shouldNotContainPrefix :: [String] -> String -> Expectation
shouldNotContainPrefix names prefix =
  any (List.isPrefixOf prefix) names `shouldBe` False

withFixtureDeadCodeProject :: (FilePath -> IO a) -> IO a
withFixtureDeadCodeProject action =
  withFixtureCopy \fixtureRoot -> do
    writeFixtureDeadCodeModules fixtureRoot
    appendFixtureDeadCodeComponents fixtureRoot
    action fixtureRoot

writeFixtureDeadCodeModules :: FilePath -> IO ()
writeFixtureDeadCodeModules fixtureRoot = do
  let srcDir = fixtureRoot </> "src" </> "DeadCode"
      appDir = fixtureRoot </> "app"
      devDir = fixtureRoot </> "src"
      testDir = fixtureRoot </> "test"
      testSupportDir = fixtureRoot </> "test" </> "TestOnly"
  createDirectoryIfMissing True srcDir
  createDirectoryIfMissing True appDir
  createDirectoryIfMissing True devDir
  createDirectoryIfMissing True testDir
  createDirectoryIfMissing True testSupportDir
  TIO.writeFile (srcDir </> "Lib.hs") deadCodeLibSource
  TIO.writeFile (srcDir </> "TypeclassMetadata.hs") deadCodeTypeclassMetadataSource
  TIO.writeFile (devDir </> "Dev.hs") deadCodeDevSource
  TIO.writeFile (appDir </> "Main.hs") deadCodeMainSource
  TIO.writeFile (testDir </> "SpecMain.hs") deadCodeTestMainSource
  TIO.writeFile (testSupportDir </> "Helper.hs") deadCodeTestSupportSource

appendFixtureDeadCodeComponents :: FilePath -> IO ()
appendFixtureDeadCodeComponents fixtureRoot = do
  TIO.appendFile (fixtureRoot </> "package.yaml") deadCodePackageSuffix
  let fixtureCabalFile = fixtureRoot </> "demo-fixture.cabal"
  fixtureCabalExists <- doesFileExist fixtureCabalFile
  if fixtureCabalExists
    then removeFile fixtureCabalFile
    else pure ()

deadCodeLibSource :: Text
deadCodeLibSource =
  T.unlines
    [ "module DeadCode.Lib where",
      "",
      "import DeadCode.TypeclassMetadata (useLiveTool)",
      "",
      "liveRoot :: Int",
      "liveRoot = liveDependency + useLiveTool",
      "",
      "liveDependency :: Int",
      "liveDependency = 1",
      "",
      "deadRoot :: Int",
      "deadRoot = deadDependency",
      "",
      "deadDependency :: Int",
      "deadDependency = 2",
      "",
      "testOnly :: Int",
      "testOnly = 3"
    ]

deadCodeTypeclassMetadataSource :: Text
deadCodeTypeclassMetadataSource =
  T.unlines
    [ "{-# LANGUAGE DataKinds #-}",
      "{-# LANGUAGE FlexibleContexts #-}",
      "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE GADTs #-}",
      "{-# LANGUAGE MultiParamTypeClasses #-}",
      "{-# LANGUAGE ScopedTypeVariables #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE AllowAmbiguousTypes #-}",
      "",
      "module DeadCode.TypeclassMetadata where",
      "",
      "import Data.Proxy (Proxy (..))",
      "import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)",
      "",
      "data FieldMetadata a metadata = FieldMetadata",
      "",
      "data Description (description :: Symbol)",
      "",
      "class IsExample a (example :: Symbol) where",
      "  exampleToJSON :: String",
      "",
      "instance KnownSymbol description => IsExample String description where",
      "  exampleToJSON = symbolVal (Proxy @description)",
      "",
      "class IsFieldMetadata a metadata where",
      "  modifySchema :: String",
      "",
      "instance IsExample String description => IsFieldMetadata String (Description description) where",
      "  modifySchema = exampleToJSON @String @description",
      "",
      "class ToSchema a where",
      "  schema :: String",
      "",
      "instance IsFieldMetadata String metadata => ToSchema (FieldMetadata String metadata) where",
      "  schema = modifySchema @String @metadata",
      "",
      "data SomeTool where",
      "  SomeTool :: ToSchema a => Proxy a -> SomeTool",
      "",
      "liveTool :: SomeTool",
      "liveTool = SomeTool (Proxy @(FieldMetadata String (Description \"x\")))",
      "",
      "useLiveTool :: Int",
      "useLiveTool =",
      "  case liveTool of",
      "    SomeTool _ -> 1"
    ]

deadCodeDevSource :: Text
deadCodeDevSource =
  T.unlines
    [ "module Dev where",
      "",
      "import DeadCode.Lib (deadRoot)",
      "",
      "runSomething :: Int",
      "runSomething = deadRoot"
    ]

deadCodeMainSource :: Text
deadCodeMainSource =
  T.unlines
    [ "module Main where",
      "",
      "import DeadCode.Lib (liveRoot)",
      "",
      "main :: IO ()",
      "main = print liveRoot"
    ]

deadCodeTestMainSource :: Text
deadCodeTestMainSource =
  T.unlines
    [ "module Main where",
      "",
      "import DeadCode.Lib (testOnly)",
      "import TestOnly.Helper (testMainHelper)",
      "",
      "main :: IO ()",
      "main = print (testOnly + testMainHelper)"
    ]

deadCodeTestSupportSource :: Text
deadCodeTestSupportSource =
  T.unlines
    [ "module TestOnly.Helper where",
      "",
      "testMainHelper :: Int",
      "testMainHelper = 4"
    ]

deadCodePackageSuffix :: Text
deadCodePackageSuffix =
  T.unlines
    [ "",
      "executables:",
      "  demo-fixture-exe:",
      "    main: Main.hs",
      "    source-dirs:",
      "    - app",
      "    dependencies:",
      "    - base",
      "    - demo-fixture",
      "",
      "tests:",
      "  demo-fixture-test:",
      "    main: SpecMain.hs",
      "    source-dirs:",
      "    - test",
      "    dependencies:",
      "    - base",
      "    - demo-fixture"
    ]

brokenEntryPackageSuffix :: Text
brokenEntryPackageSuffix =
  T.unlines
    [ "",
      "  demo-fixture-broken-exe:",
      "    main: MissingMain.hs",
      "    source-dirs:",
      "    - app",
      "    dependencies:",
      "    - base",
      "    - demo-fixture"
    ]
