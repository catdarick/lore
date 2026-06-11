module RunTestSuiteSpec
  ( spec,
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as V
import Lore.JsonRpc.Server (JsonRpcResponse (..))
import Lore.Mcp.Monad (LoreMcpMonad)
import Lore.Mcp.Protocol.Request (McpRequest (..), McpRequest'Tools (..))
import Lore.Mcp.Protocol.Server (McpServer (..), handleMcpRequest, initialMcpServerState)
import Lore.Mcp.StructuredToolRpc (structuredToolRequestHandlers)
import Lore.Mcp.Tools.RunTestSuite (runTestSuiteTool)
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import McpTestSupport (callToolWithArgs, fixtureLoreMcpAtWithCache, withFixtureCopy)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
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

    it "returns compilation-failure status via private structured calls" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        writeFile
          (fixtureRoot </> "src" </> "BrokenForCompilationStatus.hs")
          ( unlines
              [ "module BrokenForCompilationStatus where",
                "brokenValue :: Int",
                "brokenValue = \"oops\""
              ]
          )

        status <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            (_publicResponse, privateResponse) <-
              runRunTestSuitePublicAndStructuredCall (J.object [])
            structuredValue <- liftIO (extractStructuredContent privateResponse)
            liftIO (extractStructuredStatus structuredValue)

        status `shouldBe` "compilation-failure"

    it "returns invalid-arguments status via private structured calls" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        status <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            (_publicResponse, privateResponse) <-
              runRunTestSuitePublicAndStructuredCall (J.object ["testArgs" J..= ("--match \"unterminated" :: String)])
            structuredValue <- liftIO (extractStructuredContent privateResponse)
            liftIO (extractStructuredStatus structuredValue)

        status `shouldBe` "invalid-arguments"

    it "returns no-tests status via private structured calls" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        status <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            (_publicResponse, privateResponse) <-
              runRunTestSuitePublicAndStructuredCall
                (J.object ["package" J..= ("unknown-package" :: String)])
            structuredValue <- liftIO (extractStructuredContent privateResponse)
            liftIO (extractStructuredStatus structuredValue)

        status `shouldBe` "no-tests"

    it "returns tests-passed status via private structured calls" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        status <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            (_publicResponse, privateResponse) <-
              runRunTestSuitePublicAndStructuredCall (J.object [])
            structuredValue <- liftIO (extractStructuredContent privateResponse)
            liftIO (extractStructuredStatus structuredValue)

        status `shouldBe` "tests-passed"

    it "returns tests-failed status via private structured calls" do
      withFixtureCopy \fixtureRoot -> do
        addFailingFixtureTestComponent fixtureRoot
        status <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            (_publicResponse, privateResponse) <-
              runRunTestSuitePublicAndStructuredCall (J.object [])
            structuredValue <- liftIO (extractStructuredContent privateResponse)
            liftIO (extractStructuredStatus structuredValue)

        status `shouldBe` "tests-failed"

    it "keeps public and private Markdown output byte-identical for the same run" do
      withFixtureCopy \fixtureRoot -> do
        addFixtureTestComponent fixtureRoot
        (publicMarkdown, privateMarkdown) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            (publicResponse, privateResponse) <- runRunTestSuitePublicAndStructuredCall (J.object [])
            publicMarkdown <- liftIO (extractContentText publicResponse)
            privateMarkdown <- liftIO (extractContentText privateResponse)
            pure (publicMarkdown, privateMarkdown)

        publicMarkdown `shouldBe` privateMarkdown

    it "returns effectiveArguments including configured default args" do
      withScopedEnvironmentVariable "LORE_DEFAULT_TEST_ARGS" (Just "--from-config \"config value\"") do
        withFixtureCopy \fixtureRoot -> do
          addFixtureTestComponent fixtureRoot

          effectiveArguments <-
            fixtureLoreMcpAtWithCache False fixtureRoot do
              (_publicResponse, privateResponse) <-
                runRunTestSuitePublicAndStructuredCall
                  (J.object ["testArgs" J..= ("--match \"prefix sample\"" :: String)])
              structuredValue <- liftIO (extractStructuredContent privateResponse)
              liftIO (extractInvocationEffectiveArguments structuredValue)

          effectiveArguments `shouldBe` ["--from-config", "config value", "--match", "prefix sample"]

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

runRunTestSuitePublicAndStructuredCall :: J.Value -> LoreMcpMonad (JsonRpcResponse, JsonRpcResponse)
runRunTestSuitePublicAndStructuredCall args = do
  state <- liftIO initialMcpServerState
  let tools = [runTestSuiteTool]
      server =
        McpServer
          { name = "test",
            initialize = pure (),
            tools,
            customRequestHandlers = structuredToolRequestHandlers tools renderLoreDocMarkdown,
            renderer = renderLoreDocMarkdown
          }
  _ <- handleMcpRequest state server Initialize
  publicResponse <- handleMcpRequest state server (Tools (ToolsCall "runTestSuite" (Just args)))
  privateResponse <-
    handleMcpRequest
      state
      server
      (OtherRequest "lore/tools/callStructured" (Just (J.object ["name" J..= ("runTestSuite" :: String), "arguments" J..= args])))
  pure (publicResponse, privateResponse)

extractContentText :: JsonRpcResponse -> IO T.Text
extractContentText response =
  case response of
    JsonRpcResult (J.Object obj) ->
      case KM.lookup "content" obj of
        Just (J.Array contentItems)
          | Just (J.Object firstItem) <- contentItems V.!? 0,
            Just (J.String textValue) <- KM.lookup "text" firstItem ->
              pure textValue
        _ ->
          expectationFailure ("unexpected tool response payload: " <> show response) >> pure ""
    _ ->
      expectationFailure ("expected JsonRpcResult, got: " <> show response) >> pure ""

extractStructuredContent :: JsonRpcResponse -> IO J.Value
extractStructuredContent response =
  case response of
    JsonRpcResult (J.Object obj) ->
      case KM.lookup "structuredContent" obj of
        Just value -> pure value
        Nothing -> expectationFailure ("missing structuredContent in response: " <> show response) >> pure J.Null
    _ ->
      expectationFailure ("expected JsonRpcResult, got: " <> show response) >> pure J.Null

extractStructuredStatus :: J.Value -> IO T.Text
extractStructuredStatus value =
  case value of
    J.Object obj ->
      case KM.lookup "status" obj of
        Just (J.String status) -> pure status
        _ -> expectationFailure ("missing structured status in payload: " <> show value) >> pure ""
    _ -> expectationFailure ("expected structured object payload, got: " <> show value) >> pure ""

extractInvocationEffectiveArguments :: J.Value -> IO [String]
extractInvocationEffectiveArguments value =
  case value of
    J.Object obj ->
      case KM.lookup "invocation" obj of
        Just (J.Object invocationObj) ->
          case KM.lookup "effectiveArguments" invocationObj of
            Just argsValue ->
              case J.fromJSON argsValue of
                J.Error err ->
                  expectationFailure ("failed to decode effectiveArguments: " <> err <> "\nPayload: " <> show value)
                    >> pure []
                J.Success args ->
                  pure args
            _ ->
              expectationFailure ("missing invocation.effectiveArguments in payload: " <> show value) >> pure []
        _ ->
          expectationFailure ("missing invocation object in payload: " <> show value) >> pure []
    _ -> expectationFailure ("expected structured object payload, got: " <> show value) >> pure []

withScopedEnvironmentVariable :: String -> Maybe String -> IO a -> IO a
withScopedEnvironmentVariable name maybeValue action =
  bracket
    (lookupEnv name)
    restore
    (\_ -> setThenRun)
  where
    setThenRun = do
      case maybeValue of
        Nothing -> unsetEnv name
        Just value -> setEnv name value
      action

    restore previousValue =
      case previousValue of
        Nothing -> unsetEnv name
        Just value -> setEnv name value
