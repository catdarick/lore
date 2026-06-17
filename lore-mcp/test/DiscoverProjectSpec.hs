module DiscoverProjectSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Lore.Mcp.Tools.DiscoverProject (discoverProjectTool)
import McpTestSupport (callToolWithoutArgs, fixtureLoreMcp, fixtureLoreMcpAtWithCache, withFixtureCopy)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  describe "discoverProject" do
    it "renders package and component paths relative to the project root" do
      discoveryResult <-
        fixtureLoreMcp do
          callToolWithoutArgs discoverProjectTool

      discoveryResult `shouldContainText` "Package: demo-fixture"
      discoveryResult `shouldContainText` "package root: ./"
      discoveryResult `shouldContainText` "package manifest: demo-fixture.cabal"
      discoveryResult `shouldContainText` "source dirs: src/"

    it "separates workspace-, package-, and component-specific configuration" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          writeFixturePackageYaml fixtureRoot
          writeSecondFixturePackage fixtureRoot
          addSecondPackageToProject fixtureRoot
          createDirectoryIfMissing True (fixtureRoot </> "app")
          writeFile (fixtureRoot </> "app" </> "Main.hs") "module Main where\n\nmain :: IO ()\nmain = pure ()\n"
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithoutArgs discoverProjectTool

      discoveryResult `shouldContainText` "# Workspace"
      discoveryResult `shouldContainText` "shared dependencies: base"
      discoveryResult `shouldContainText` "shared GHC options: -Wall"
      discoveryResult `shouldContainText` "shared extensions: KindSignatures, TypeFamilies"
      T.count "KindSignatures, TypeFamilies" discoveryResult `shouldBe` 1
      discoveryResult `shouldContainText` "package shared dependencies: containers"
      discoveryResult `shouldContainText` "package shared GHC options: -Wcompat"
      discoveryResult `shouldContainText` "package shared extensions: GADTs"
      discoveryResult `shouldContainText` "### Component: library"
      discoveryResult `shouldNotContainText` "component specific dependencies: (none)"
      discoveryResult `shouldNotContainText` "component specific GHC options: (none)"
      discoveryResult `shouldNotContainText` "component specific extensions: (none)"
      discoveryResult `shouldNotContainText` "package shared GHC options: (none)"
      discoveryResult `shouldContainText` "### Component: executable:demo-cli"
      discoveryResult `shouldContainText` "source dirs: app/"
      discoveryResult `shouldContainText` "main module: app/Main.hs"
      discoveryResult `shouldContainText` "component specific dependencies: text"
      discoveryResult `shouldContainText` "component specific GHC options: -threaded"
      discoveryResult `shouldContainText` "component specific extensions: LambdaCase"

shouldNotContainText :: T.Text -> T.Text -> Expectation
shouldNotContainText actual unexpected =
  if T.isInfixOf unexpected actual
    then
      expectationFailure
        ( "Unexpected snippet: "
            <> T.unpack unexpected
            <> "\n\nFull output:\n"
            <> T.unpack actual
        )
    else pure ()

shouldContainText :: T.Text -> T.Text -> Expectation
shouldContainText actual expected =
  if T.isInfixOf expected actual
    then pure ()
    else
      expectationFailure
        ( "Missing expected snippet: "
            <> T.unpack expected
            <> "\n\nFull output:\n"
            <> T.unpack actual
        )

writeFixturePackageYaml :: FilePath -> IO ()
writeFixturePackageYaml fixtureRoot =
  writeFile
    (fixtureRoot </> "package.yaml")
    ( unlines
        [ "name: demo-fixture",
          "version: 0.1.0.0",
          "",
          "dependencies:",
          "- base >= 4.7 && < 5",
          "- containers",
          "",
          "ghc-options:",
          "- -Wall",
          "- -Wcompat",
          "",
          "default-extensions:",
          "- TypeFamilies",
          "- KindSignatures",
          "- GADTs",
          "",
          "library:",
          "  source-dirs: src",
          "",
          "executables:",
          "  demo-cli:",
          "    main: Main.hs",
          "    source-dirs: app",
          "    ghc-options:",
          "    - -threaded",
          "    dependencies:",
          "    - text",
          "    default-extensions:",
          "    - LambdaCase"
        ]
    )

writeSecondFixturePackage :: FilePath -> IO ()
writeSecondFixturePackage fixtureRoot = do
  let packageRoot = fixtureRoot </> "support"
  createDirectoryIfMissing True (packageRoot </> "src")
  writeFile
    (packageRoot </> "package.yaml")
    ( unlines
        [ "name: demo-support",
          "version: 0.1.0.0",
          "",
          "dependencies:",
          "- base >= 4.7 && < 5",
          "",
          "ghc-options:",
          "- -Wall",
          "",
          "default-extensions:",
          "- TypeFamilies",
          "- KindSignatures",
          "",
          "library:",
          "  source-dirs: src"
        ]
    )
  writeFile (packageRoot </> "src" </> "Support.hs") "module Support where\n"

addSecondPackageToProject :: FilePath -> IO ()
addSecondPackageToProject fixtureRoot = do
  stackProjectExists <- doesFileExist (fixtureRoot </> "stack.yaml")
  if stackProjectExists
    then appendFile (fixtureRoot </> "stack.yaml") "- support\n"
    else writeFile (fixtureRoot </> "cabal.project") "packages:\n  .\n  support\n"
