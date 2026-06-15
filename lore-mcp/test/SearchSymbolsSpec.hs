module SearchSymbolsSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JKM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Unique as GHC.Unique
import Lore.Internal.Lookup.Types (Symbol (..), SymbolSuggestion (..), SymbolVisibility (..))
import Lore.Mcp.Internal.Tool (SomeTool, getSomeToolSpec)
import Lore.Mcp.Monad (LoreMcpMonad)
import Lore.Mcp.Tools.SearchSymbols (searchSymbolsTool)
import Lore.Tools.Internal.SymbolSuggestions (GroupedSymbolSuggestion (..), groupSymbolSuggestions)
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

    it "accepts an empty modulePatterns array as unrestricted" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs searchSymbolsTool (searchSymbolsArgsWithPatterns "supportValues" [])

      searchResult `shouldContainText` "similar symbols for \"supportValues\":"
      searchResult `shouldContainText` "supportValues"

    it "accepts null modulePatterns as unrestricted" do
      searchResult <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs searchSymbolsTool (searchSymbolsArgsWithNullPatterns "supportValues")

      searchResult `shouldContainText` "similar symbols for \"supportValues\":"
      searchResult `shouldContainText` "supportValues"

    it "filters symbols by a single module pattern and renders the scope" do
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
            callToolWithArgs searchSymbolsTool (searchSymbolsArgsWithPatterns "create" ["Demo.Support"])

      searchResult `shouldContainText` "similar symbols for \"create\" in modules matching \"Demo.Support\":"
      searchResult `shouldContainText` "Demo.Support.create"

    it "passes multiple module patterns to core search with OR semantics" do
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
            callToolWithArgs searchSymbolsTool (searchSymbolsArgsWithPatterns "create" ["Missing.*", "Demo.Support"])

      searchResult `shouldContainText` "similar symbols for \"create\" in modules matching any of:\n\"Missing.*\", \"Demo.Support\":"
      searchResult `shouldContainText` "Demo.Support.create"

    it "rejects empty module pattern items" do
      ( fixtureLoreMcp do
          callToolWithArgs searchSymbolsTool (searchSymbolsArgsWithPatterns "create" [""])
        )
        `shouldThrow` errorCall "modulePatterns items must be nonempty strings"

    it "uses the plural modulePatterns schema field" do
      let toolSpec = getSomeToolSpec (searchSymbolsTool :: SomeTool LoreMcpMonad)

      toolSpecHasProperty "modulePatterns" toolSpec `shouldBe` True
      toolSpecHasProperty "modulePattern" toolSpec `shouldBe` False

    it "groups ranked suggestions before applying a rendered limit" do
      let grouped =
            take 2 $
              groupSymbolSuggestions
                [ suggestion 1 "Module.A" "foo",
                  suggestion 2 "Module.B" "foo",
                  suggestion 3 "Module.C" "bar",
                  suggestion 4 "Module.D" "baz"
                ]

      map (.groupedLookupName) grouped `shouldBe` ["foo", "bar"]
      map (.groupedDefiningModules) grouped `shouldBe` [["Module.A", "Module.B"], ["Module.C"]]

    it "does not rely on overfetching when many symbols share one lookup name" do
      let grouped =
            take 2 $
              groupSymbolSuggestions $
                [suggestion unique ("Module.Foo" <> show unique) "foo" | unique <- [1 .. 25]]
                  <> [suggestion 100 "Module.Bar" "bar"]
                  <> [suggestion 101 "Module.Baz" "baz"]

      map (.groupedLookupName) grouped `shouldBe` ["foo", "bar"]

searchSymbolsArgs :: Text -> J.Value
searchSymbolsArgs query =
  J.object
    [ "query" J..= query
    ]

searchSymbolsArgsWithPatterns :: Text -> [Text] -> J.Value
searchSymbolsArgsWithPatterns query modulePatterns =
  J.object
    [ "query" J..= query,
      "modulePatterns" J..= modulePatterns
    ]

searchSymbolsArgsWithNullPatterns :: Text -> J.Value
searchSymbolsArgsWithNullPatterns query =
  J.object
    [ "query" J..= query,
      "modulePatterns" J..= J.Null
    ]

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
  T.isInfixOf expected actual `shouldBe` True

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText actual expected =
  T.isInfixOf expected actual `shouldBe` False

toolSpecHasProperty :: Text -> J.Value -> Bool
toolSpecHasProperty propertyName = \case
  J.Object toolSpec ->
    case JKM.lookup "inputSchema" toolSpec of
      Just (J.Object inputSchema) ->
        case JKM.lookup "properties" inputSchema of
          Just (J.Object properties) ->
            JK.fromText propertyName `JKM.member` properties
          _ -> False
      _ -> False
  _ -> False

suggestion :: Integer -> String -> String -> SymbolSuggestion
suggestion unique moduleName occName =
  SymbolSuggestion
    { suggestedSymbol = testSymbol unique moduleName occName,
      suggestedLookupName = T.pack occName,
      suggestionExactLookupNameMatch = False,
      suggestionScore = 1.0,
      suggestionEvidence = []
    }

testSymbol :: Integer -> String -> String -> Symbol
testSymbol unique moduleName occName =
  Symbol
    { name = GHC.mkExternalName (GHC.Unique.mkUniqueGrimily (fromInteger unique)) (testModule moduleName) (GHC.mkVarOcc occName) GHC.noSrcSpan,
      visibility = Symbol'ExportedFrom (Set.singleton (testModule moduleName)),
      aliases = Set.empty
    }

testModule :: String -> GHC.Module
testModule moduleName =
  GHC.mkModule GHC.mainUnit (GHC.mkModuleName moduleName)
