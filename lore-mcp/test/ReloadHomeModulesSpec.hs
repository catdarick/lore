module ReloadHomeModulesSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import qualified Data.Text as T
import Lore.Mcp.Tools.ReloadHomeModules (reloadHomeModulesTool)
import qualified Lore.Tools.ReloadHomeModules as ToolsReload
import McpTestSupport (callToolWithArgs, fixtureLoreMcpAtWithCache, withFixtureCopy)
import System.FilePath ((</>))
import Test.Hspec

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
          truncatedMessage = ToolsReload.truncateDiagnosticMessage longMessage
      T.length truncatedMessage `shouldBe` 700

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
