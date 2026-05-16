module SearchSymbolsSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Mcp.Tools.SearchSymbols (searchSymbolsTool)
import McpTestSupport (callToolWithArgs, fixtureLoreMcp, loadFixtureTargets)
import Test.Hspec

spec :: Spec
spec =
  describe "searchSymbols" do
    it "returns not-loaded message before targets are loaded" do
      searchResult <-
        fixtureLoreMcp do
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "supportValues")

      searchResult `shouldBe` "Targets have not been loaded yet. Run reloadHomeModules first."

    it "returns fuzzy suggestions for misspelled query symbols" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureTargets
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "supportVlaues")

      searchResult `shouldContainText` "Found "
      searchResult `shouldContainText` "similar symbols for \"supportVlaues\":"
      searchResult `shouldContainText` "supportValues"
      searchResult `shouldContainText` "Demo.Support.supportValues"

    it "searches similar symbols even when exact symbols exist" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureTargets
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "supportValues")

      searchResult `shouldContainText` "similar symbols for \"supportValues\":"
      searchResult `shouldContainText` "supportValues"

    it "renders single-module suggestions as fully qualified symbol names" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureTargets
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "usageInfo")

      searchResult `shouldContainText` "System.Console.GetOpt.usageInfo"
      searchResult `shouldNotContainText` "usageInfo (defined in: "

    it "merges same-occurrence symbols and summarizes defining modules" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureTargets
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "map")

      searchResult `shouldContainText` "Found 10 similar symbols for \"map\":"
      searchResult `shouldContainText` "map (defined in: "

searchSymbolsArgs :: Text -> J.Value
searchSymbolsArgs query =
  J.object
    [ "query" J..= query
    ]

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
  T.isInfixOf expected actual `shouldBe` True

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText actual expected =
  T.isInfixOf expected actual `shouldBe` False
