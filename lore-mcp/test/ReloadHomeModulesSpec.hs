module ReloadHomeModulesSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Lore.Mcp.Tools.ReloadHomeModules (reloadHomeModulesTool)
import McpTestSupport (callToolWithoutArgs, fixtureLoreMcpAtWithCache, withFixtureCopy)
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
            callToolWithoutArgs reloadHomeModulesTool

        result `shouldContainText` "Failed to load"
        result `shouldContainText` "BrokenWarnError.hs"
        result `shouldContainText` ("## " <> T.pack brokenFile)
        result `shouldContainText` "### Error"
        result `shouldContainText` "^"
        result `shouldNotContainText` "1. error:"

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
