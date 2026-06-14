module ReloadHomeModulesSpec
  ( spec,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Lore (HomeModulesLoadSummary (..), LoadHomeModulesResult (..))
import Lore.JsonRpc.Server (JsonRpcResponse (..))
import Lore.Mcp.Monad (LoreMcpMonad)
import Lore.Mcp.Protocol.Request (McpRequest (..), McpRequest'Tools (..))
import Lore.Mcp.Protocol.Server (McpServer (..), handleMcpRequest, initialMcpServerState)
import Lore.Mcp.StructuredToolRpc (structuredToolRequestHandlers)
import Lore.Mcp.Tools.ReloadHomeModules (reloadHomeModulesTool)
import Lore.Tools.ReloadHomeModules
  ( ReloadHomeModulesOptions (..),
    ReloadHomeModulesStatus (..),
    reloadHomeModules,
    reloadHomeModulesStatus,
    renderReloadHomeModulesResult,
    truncateDiagnosticMessage,
  )
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import Lore.Tools.Result (RenderedResult (..))
import McpTestSupport (callToolWithArgs, fixtureLoreMcpAtWithCache, withFixtureCopy)
import System.FilePath ((</>))
import Test.Hspec

data ReloadStructuredContent = ReloadStructuredContent
  { status :: T.Text,
    loadedModules :: Int,
    failedModules :: Int,
    totalModules :: Int,
    autofixedModules :: Int,
    autofixedFiles :: [FilePath]
  }
  deriving stock (Eq, Show, Generic)

instance J.FromJSON ReloadStructuredContent

spec :: Spec
spec =
  describe "reloadHomeModules" do
    it "renders grouped diagnostics with file and severity headings plus snippets" do
      withFixtureCopy \fixtureRoot -> do
        let brokenFile = fixtureRoot </> "src" </> "BrokenWarnError.hs"
        writeFile
          brokenFile
          ( unlines
              [ "module BrokenWarnError where",
                "",
                "shadowExample :: Int -> Int",
                "shadowExample value =",
                "  let value = 1",
                "   in value",
                "",
                "brokenValue :: Int",
                "brokenValue = \"oops\""
              ]
          )

        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs reloadHomeModulesTool (J.object [])

        result `shouldContainText` "Failed to load"
        result `shouldContainText` "BrokenWarnError.hs"
        result `shouldContainText` ("## " <> T.pack brokenFile)
        result `shouldContainText` "### Error"
        result `shouldContainText` "^"
        result `shouldNotContainText` "1. error:"

    it "prints next-page skip hint only when there are remaining diagnostics" do
      withFixtureCopy \fixtureRoot -> do
        mapM_ (writeBrokenModule fixtureRoot) [1 .. 6 :: Int]

        firstPage <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs reloadHomeModulesTool (J.object [])

        secondPage <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs reloadHomeModulesTool (J.object ["skip" J..= (5 :: Int)])

        firstPage `shouldContainText` "If you don't have enough context to fix the listed errors, set skip to 5 to get the next page."
        secondPage `shouldNotContainText` "If you don't have enough context to fix the listed errors, set skip to "

    it "truncates diagnostic message text over 700 symbols" do
      let longMessage = T.replicate 1200 "x"
          truncatedMessage = truncateDiagnosticMessage longMessage
      T.length truncatedMessage `shouldBe` 700

    it "returns structured producer data for successful reloads and keeps Markdown unchanged" do
      withFixtureCopy \fixtureRoot -> do
        (status, loadedModules, failedModules, totalModules, renderedMarkdown, rerenderedMarkdown) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            renderedResult <-
              reloadHomeModules
                ReloadHomeModulesOptions
                  { reloadHomeModulesDiagnosticsPageRequest = Nothing
                  }
            let loadResult = renderedResultValue renderedResult
                renderedDoc = renderedResultDocument renderedResult
            rerendered <- renderReloadHomeModulesResult loadResult
            pure
              ( reloadHomeModulesStatus loadResult,
                loadHomeModulesLoaded loadResult,
                loadHomeModulesFailed loadResult,
                loadHomeModulesTotal loadResult,
                renderLoreDocMarkdown renderedDoc,
                renderLoreDocMarkdown rerendered
              )

        status `shouldBe` ReloadHomeModulesStatusSuccess
        loadedModules + failedModules `shouldBe` totalModules
        renderedMarkdown `shouldBe` rerenderedMarkdown

    it "returns structured producer data for compilation failures" do
      withFixtureCopy \fixtureRoot -> do
        writeBrokenModule fixtureRoot 999

        (status, failedModules) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            renderedResult <-
              reloadHomeModules
                ReloadHomeModulesOptions
                  { reloadHomeModulesDiagnosticsPageRequest = Nothing
                  }
            let loadResult = renderedResultValue renderedResult
            pure
              ( reloadHomeModulesStatus loadResult,
                loadHomeModulesFailed loadResult
              )

        status `shouldBe` ReloadHomeModulesStatusCompilationFailure
        failedModules `shouldSatisfy` (> 0)

    it "preserves autofix file information in the structured producer result" do
      withFixtureCopy \fixtureRoot -> do
        writeFile
          (fixtureRoot </> "src" </> "AutoFixUnusedImport.hs")
          ( unlines
              [ "module AutoFixUnusedImport where",
                "",
                "import Data.List (nub)",
                "",
                "values :: [Int]",
                "values = [1, 2, 3]"
              ]
          )

        (autofixedCount, autofixedFiles, summaryFiles) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            renderedResult <-
              reloadHomeModules
                ReloadHomeModulesOptions
                  { reloadHomeModulesDiagnosticsPageRequest = Nothing
                  }
            let loadResult = renderedResultValue renderedResult
            pure
              ( loadHomeModulesAutofixed loadResult,
                loadHomeModulesAutofixedFiles loadResult,
                map fst (loadHomeModulesAutofixSummaryByFile loadResult)
              )

        autofixedCount `shouldBe` length autofixedFiles
        Set.fromList autofixedFiles `shouldBe` Set.fromList summaryFiles

    it "returns private structured status for successful reloads and preserves public Markdown" do
      withFixtureCopy \fixtureRoot -> do
        (publicMarkdown, privateMarkdown, structuredContent) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            (publicResponse, privateResponse) <- runReloadPublicAndStructuredCall
            publicMarkdown <- liftIO (extractContentText publicResponse)
            privateMarkdown <- liftIO (extractContentText privateResponse)
            structuredContent <- liftIO (decodeStructuredReload privateResponse)
            pure (publicMarkdown, privateMarkdown, structuredContent)

        publicMarkdown `shouldBe` privateMarkdown
        structuredContent.status `shouldBe` "success"
        structuredContent.loadedModules + structuredContent.failedModules `shouldBe` structuredContent.totalModules

    it "returns private structured compilation-failure status for failed reloads" do
      withFixtureCopy \fixtureRoot -> do
        writeBrokenModule fixtureRoot 1000

        structuredContent <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            (_publicResponse, privateResponse) <- runReloadPublicAndStructuredCall
            liftIO (decodeStructuredReload privateResponse)

        structuredContent.status `shouldBe` "compilation-failure"
        structuredContent.failedModules `shouldSatisfy` (> 0)

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

shouldNotContainText :: T.Text -> T.Text -> Expectation
shouldNotContainText actual unexpected =
  if T.isInfixOf unexpected actual
    then
      expectationFailure
        ( "Unexpected snippet found: "
            <> T.unpack unexpected
            <> "\n\nFull output:\n"
            <> T.unpack actual
        )
    else pure ()

writeBrokenModule :: FilePath -> Int -> IO ()
writeBrokenModule fixtureRoot index =
  writeFile
    (fixtureRoot </> "src" </> ("BrokenForPagination" <> show index <> ".hs"))
    ( unlines
        [ "module BrokenForPagination" <> show index <> " where",
          "brokenValue :: Int",
          "brokenValue = \"oops\""
        ]
    )

runReloadPublicAndStructuredCall :: LoreMcpMonad (JsonRpcResponse, JsonRpcResponse)
runReloadPublicAndStructuredCall = do
  state <- liftIO initialMcpServerState
  let tools = [reloadHomeModulesTool]
      server =
        McpServer
          { name = "test",
            initialize = pure (),
            tools,
            customRequestHandlers = structuredToolRequestHandlers tools renderLoreDocMarkdown,
            renderer = renderLoreDocMarkdown
          }
      args = J.object []
  _ <- handleMcpRequest state server Initialize
  publicResponse <- handleMcpRequest state server (Tools (ToolsCall "reloadHomeModules" (Just args)))
  privateResponse <-
    handleMcpRequest
      state
      server
      (OtherRequest "lore/tools/callStructured" (Just (J.object ["name" J..= ("reloadHomeModules" :: String), "arguments" J..= args])))
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

decodeStructuredReload :: JsonRpcResponse -> IO ReloadStructuredContent
decodeStructuredReload response =
  case response of
    JsonRpcResult (J.Object obj) ->
      case KM.lookup "structuredContent" obj of
        Just structuredValue ->
          case J.fromJSON structuredValue of
            J.Error err ->
              expectationFailure ("failed to decode structuredContent: " <> err <> "\nResponse: " <> show response)
                >> pure (ReloadStructuredContent "" 0 0 0 0 [])
            J.Success decoded ->
              pure decoded
        Nothing ->
          expectationFailure ("missing structuredContent field in response: " <> show response)
            >> pure (ReloadStructuredContent "" 0 0 0 0 [])
    _ ->
      expectationFailure ("expected JsonRpcResult, got: " <> show response) >> pure (ReloadStructuredContent "" 0 0 0 0 [])

loadSummary :: LoadHomeModulesResult -> HomeModulesLoadSummary
loadSummary (LoadHomeModulesCompleted summary) = summary
loadSummary (LoadHomeModulesPreparationFailed failure) = error ("Expected completed load, got preparation failure: " <> show failure)

loadHomeModulesLoaded :: LoadHomeModulesResult -> Int
loadHomeModulesLoaded = (.homeModulesLoaded) . loadSummary

loadHomeModulesFailed :: LoadHomeModulesResult -> Int
loadHomeModulesFailed = (.homeModulesFailed) . loadSummary

loadHomeModulesTotal :: LoadHomeModulesResult -> Int
loadHomeModulesTotal = (.homeModulesTotal) . loadSummary

loadHomeModulesAutofixed :: LoadHomeModulesResult -> Int
loadHomeModulesAutofixed = (.homeModulesAutofixed) . loadSummary

loadHomeModulesAutofixedFiles :: LoadHomeModulesResult -> [FilePath]
loadHomeModulesAutofixedFiles = (.homeModulesAutofixedFiles) . loadSummary

loadHomeModulesAutofixSummaryByFile :: LoadHomeModulesResult -> [(FilePath, [String])]
loadHomeModulesAutofixSummaryByFile = (.homeModulesAutofixSummaryByFile) . loadSummary
