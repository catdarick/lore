module FindDeadCodeSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Mcp.Tools.FindDeadCode (findDeadCodeTool)
import McpTestSupport
  ( callToolWithArgs,
    fixtureLoreMcpAtWithCache,
    loadFixtureHomeModules,
    withFixtureCopy,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "findDeadCode" do
    it "reports dead definitions reachable from executable roots only" do
      result <- runFindDeadCodeFixture (J.object [])
      result `shouldContainText` "deadRoot"
      result `shouldContainText` "deadDependency"
      result `shouldContainText` "testOnly"
      result `shouldNotContainText` "liveRoot"
      result `shouldNotContainText` "liveDependency"
      result `shouldNotContainText` "ToSchema"
      result `shouldNotContainText` "schema"
      result `shouldNotContainText` "IsFieldMetadata"
      result `shouldNotContainText` "modifySchema"
      result `shouldNotContainText` "IsExample"
      result `shouldNotContainText` "exampleToJSON"
      result `shouldNotContainText` "$fToSchema"
      result `shouldNotContainText` "$fIsFieldMetadata"
      result `shouldNotContainText` "$fIsExample"
      result `shouldNotContainText` "testMainHelper"

    it "supports module filtering" do
      result <-
        runFindDeadCodeFixture $
          J.object
            [ "modules" J..= ["DeadCode.Lib" :: Text]
            ]
      result `shouldContainText` "deadRoot"
      result `shouldContainText` "deadDependency"
      result `shouldNotContainText` "Dev.runSomething"

    it "supports aliveModules roots" do
      result <-
        runFindDeadCodeFixtureWithConfig "alive-modules:\n  - Dev\n" (J.object [])
      result `shouldNotContainText` "deadRoot"
      result `shouldNotContainText` "deadDependency"

    it "supports aliveSymbols roots" do
      result <-
        runFindDeadCodeFixtureWithConfig "alive-symbols:\n  - DeadCode.Lib.deadRoot\n" (J.object [])
      result `shouldNotContainText` "deadRoot"
      result `shouldNotContainText` "deadDependency"

    it "returns a clear error for invalid lore.yaml content" do
      result <-
        runFindDeadCodeFixtureWithConfig "alive-symbols: [\n" (J.object [])
      result `shouldContainText` "Failed to parse \"lore.yaml\""

    it "returns a clear error for unresolved module names" do
      result <-
        runFindDeadCodeFixture $
          J.object
            [ "modules" J..= ["Missing.Module" :: Text]
            ]
      result `shouldContainText` "is not present in the loaded home module graph"

    it "applies pagination" do
      result <-
        runFindDeadCodeFixture $
          J.object
            [ "skip" J..= (1 :: Int)
            ]
      result `shouldContainText` "Showing 29 of"
      result `shouldNotContainText` "DeadCode.Lib.deadRoot"

    it "renders warnings when entry modules cannot be resolved" do
      result <-
        runFindDeadCodeFixtureWithBrokenEntry (J.object [])
      result `shouldContainText` "Warning: Failed to resolve entry module for"

runFindDeadCodeFixture :: J.Value -> IO Text
runFindDeadCodeFixture args =
  withFixtureCopy \fixtureRoot -> do
    writeFixtureDeadCodeModules fixtureRoot
    appendFixtureDeadCodeComponents fixtureRoot
    fixtureLoreMcpAtWithCache False fixtureRoot do
      loadFixtureHomeModules
      callToolWithArgs findDeadCodeTool args

runFindDeadCodeFixtureWithConfig :: Text -> J.Value -> IO Text
runFindDeadCodeFixtureWithConfig configSource args =
  withFixtureCopy \fixtureRoot -> do
    writeFixtureDeadCodeModules fixtureRoot
    appendFixtureDeadCodeComponents fixtureRoot
    TIO.writeFile (fixtureRoot </> "lore.yaml") configSource
    fixtureLoreMcpAtWithCache False fixtureRoot do
      loadFixtureHomeModules
      callToolWithArgs findDeadCodeTool args

runFindDeadCodeFixtureWithBrokenEntry :: J.Value -> IO Text
runFindDeadCodeFixtureWithBrokenEntry args =
  withFixtureCopy \fixtureRoot -> do
    writeFixtureDeadCodeModules fixtureRoot
    appendFixtureDeadCodeComponents fixtureRoot
    TIO.appendFile (fixtureRoot </> "package.yaml") brokenEntryPackageSuffix
    fixtureLoreMcpAtWithCache False fixtureRoot do
      loadFixtureHomeModules
      callToolWithArgs findDeadCodeTool args

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

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle =
  T.unpack haystack `shouldContain` T.unpack needle

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText haystack needle =
  T.unpack haystack `shouldNotContain` T.unpack needle
