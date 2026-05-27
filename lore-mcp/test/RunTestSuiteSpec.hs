module RunTestSuiteSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import qualified Data.Text as T
import Lore.Mcp.Tools.RunTestSuite (runTestSuiteTool)
import McpTestSupport (callToolWithArgs, fixtureLoreMcpAtWithCache, withFixtureCopy)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  describe "runTestSuite" do
    it "runs discovered test mains (including generated Main modules) and forwards withArgs" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs
              runTestSuiteTool
              ( J.object
                  [ "testArgs" J..= ("--match \"prefix sample\"" :: String)
                  ]
              )

        result `shouldContainText` "Executed 1 test components"
        result `shouldContainText` "[PASS] demo-fixture/test:fixture-test"
        result `shouldContainText` "fixture-test-args=[\"--match\",\"prefix sample\"]"

    it "supports package filter and reports when no test components match" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs
              runTestSuiteTool
              ( J.object
                  [ "package" J..= ("unknown-package" :: String)
                  ]
              )

        result `shouldContainText` "No test components found for package \"unknown-package\"."

    it "renders reload diagnostics and skips test execution when not all modules load" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        writeFile
          (fixtureRoot </> "src" </> "BrokenForPartialLoad.hs")
          ( unlines
              [ "module BrokenForPartialLoad where",
                "brokenValue :: Int",
                "brokenValue = \"oops\""
              ]
          )
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs
              runTestSuiteTool
              ( J.object
                  [ "package" J..= ("unknown-package" :: String)
                  ]
              )

        result `shouldContainText` "Failed to load "
        result `shouldContainText` "BrokenForPartialLoad.hs"

    it "reloads home modules implicitly when tests are run before explicit reload" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs runTestSuiteTool (J.object [])

        result `shouldContainText` "Executed 1 test components"

    it "includes captured test output in execution failures" do
      withFixtureCopy \fixtureRoot -> do
        addFailingFixtureTestComponent fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs runTestSuiteTool (J.object [])

        result `shouldContainText` "[FAIL] demo-fixture/test:fixture-test"
        result `shouldContainText` T.pack (show (ExitFailure 1))
        result `shouldContainText` "Captured output:"
        result `shouldContainText` "fixture-test-failure-signal"

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

addFixtureTestComponent :: FilePath -> IO ()
addFixtureTestComponent fixtureRoot = do
  writeFixturePackageYaml fixtureRoot
  createDirectoryIfMissing True (fixtureRoot </> "test")
  writeFile
    (fixtureRoot </> "test" </> "Spec.hs")
    ( unlines
        [ "module Main (main) where",
          "",
          "import System.Environment (getArgs)",
          "",
          "main :: IO ()",
          "main = do",
          "  args <- getArgs",
          "  putStrLn (\"fixture-test-args=\" ++ show args)"
        ]
    )

addFailingFixtureTestComponent :: FilePath -> IO ()
addFailingFixtureTestComponent fixtureRoot = do
  writeFixturePackageYaml fixtureRoot
  createDirectoryIfMissing True (fixtureRoot </> "test")
  writeFile
    (fixtureRoot </> "test" </> "Spec.hs")
    ( unlines
        [ "module Main (main) where",
          "",
          "import System.Exit (exitFailure)",
          "",
          "main :: IO ()",
          "main = do",
          "  putStrLn \"fixture-test-failure-signal\"",
          "  exitFailure"
        ]
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
          "default-extensions:",
          "- TypeFamilies",
          "- KindSignatures",
          "",
          "library:",
          "  source-dirs: src",
          "",
          "tests:",
          "  fixture-test:",
          "    main: Spec.hs",
          "    source-dirs: test",
          "    dependencies:",
          "    - base",
          "    - demo-fixture"
        ]
    )
