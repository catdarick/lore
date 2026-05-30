module DiscoverProjectSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Lore.Mcp.Tools.DiscoverProject (discoverProjectTool)
import McpTestSupport (callToolWithoutArgs, fixtureLoreMcp, fixtureLoreMcpAtWithCache, withFixtureCopy)
import System.Directory (createDirectoryIfMissing)
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
      discoveryResult `shouldContainText` "package manifest: package.yaml"
      discoveryResult `shouldContainText` "source dirs: src/"

    it "separates shared and component-specific dependencies/extensions" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          writeFixturePackageYaml fixtureRoot
          createDirectoryIfMissing True (fixtureRoot </> "app")
          writeFile (fixtureRoot </> "app" </> "Main.hs") "module Main where\n\nmain :: IO ()\nmain = pure ()\n"
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithoutArgs discoverProjectTool

      discoveryResult `shouldContainText` "shared dependencies: base, containers"
      discoveryResult `shouldContainText` "shared GHC options: -Wall"
      discoveryResult `shouldContainText` "shared extensions: KindSignatures, TypeFamilies"
      discoveryResult `shouldContainText` "### Component: library"
      discoveryResult `shouldContainText` "component specific dependencies: (none)"
      discoveryResult `shouldContainText` "component specific GHC options: (none)"
      discoveryResult `shouldContainText` "### Component: executable:demo-cli"
      discoveryResult `shouldContainText` "source dirs: app/"
      discoveryResult `shouldContainText` "main module: app/Main.hs"
      discoveryResult `shouldContainText` "component specific dependencies: text"
      discoveryResult `shouldContainText` "component specific GHC options: -threaded"
      discoveryResult `shouldContainText` "component specific extensions: LambdaCase"

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
          "",
          "default-extensions:",
          "- TypeFamilies",
          "- KindSignatures",
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
