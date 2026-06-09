module SearchSymbolsSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Mcp.Tools.SearchSymbols (searchSymbolsTool)
import McpTestSupport (callToolWithArgs, fixtureLoreMcp, fixtureLoreMcpAtWithCache, loadFixtureHomeModules, withFixtureCopy)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  describe "searchSymbols" do
    it "auto-loads home modules on first call" do
      searchResult <-
        fixtureLoreMcp do
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "supportValues")

      searchResult `shouldContainText` "similar symbols for \"supportValues\":"
      searchResult `shouldNotContainText` "Home modules have not been loaded yet."

    it "returns fuzzy suggestions for misspelled query symbols" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "supportVlaues")

      searchResult `shouldContainText` "Found "
      searchResult `shouldContainText` "similar symbols for \"supportVlaues\":"
      searchResult `shouldContainText` "supportValues"
      searchResult `shouldContainText` "Demo.Support.supportValues"

    it "searches similar symbols even when exact symbols exist" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "supportValues")

      searchResult `shouldContainText` "similar symbols for \"supportValues\":"
      searchResult `shouldContainText` "supportValues"

    it "renders single-module suggestions as fully qualified symbol names" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "usageInfo")

      searchResult `shouldContainText` "System.Console.GetOpt.usageInfo"
      searchResult `shouldNotContainText` "usageInfo (defined in: "

    it "merges same-occurrence symbols and summarizes defining modules" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs searchSymbolsTool (searchSymbolsArgs "map")

      searchResult `shouldContainText` "Found 10 similar symbols for \"map\":"
      searchResult `shouldContainText` "map (defined in: "

    it "renders module-assisted symbol ordering from core search" do
      searchResult <-
        withFixtureCopy \fixtureRoot -> do
          appendFile
            (fixtureRoot </> "src" </> "Demo.hs")
            "\ncreate :: Int -> Int\ncreate value = value + 1\n"
          appendFile
            (fixtureRoot </> "src" </> "Demo" </> "Support.hs")
            "\ncreate :: Int -> Int\ncreate value = value + supportSeed\n"
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs searchSymbolsTool (searchSymbolsArgs "createSupport")

      searchResult `shouldContainText` "create (defined in: Demo.Support, Demo)"

    it "renders type-context symbol ordering from core search" do
      searchResult <-
        withFixtureCopy \fixtureRoot -> do
          appendFile
            (fixtureRoot </> "src" </> "Demo.hs")
            "\ndata UserAccount = UserAccount\n\ncreate :: Int -> UserAccount\ncreate _ = UserAccount\n"
          appendFile
            (fixtureRoot </> "src" </> "Demo" </> "Support.hs")
            "\ndata DiscountAccount = DiscountAccount\n\ncreate :: Int -> DiscountAccount\ncreate _ = DiscountAccount\n"
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs searchSymbolsTool (searchSymbolsArgs "createDiscountAccount")

      searchResult `shouldContainText` "create (defined in: Demo.Support, Demo)"

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
