module SearchSymbolsSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JKM
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Mcp.Internal.Tool (SomeTool, getSomeToolSpec)
import Lore.Mcp.Monad (LoreMcpMonad)
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

toolSpecArrayItemHasMinLength :: Text -> Integer -> J.Value -> Bool
toolSpecArrayItemHasMinLength propertyName expectedMinLength = \case
  J.Object toolSpec ->
    case JKM.lookup "inputSchema" toolSpec >>= valueObjectField "properties" >>= objectField propertyName >>= objectField "items" >>= JKM.lookup "minLength" of
      Just actualMinLength -> actualMinLength == J.Number (fromInteger expectedMinLength)
      Nothing -> False
  _ -> False

valueObjectField :: Text -> J.Value -> Maybe J.Object
valueObjectField fieldName = \case
  J.Object object -> objectField fieldName object
  _ -> Nothing

objectField :: Text -> J.Object -> Maybe J.Object
objectField fieldName object =
  case JKM.lookup (JK.fromText fieldName) object of
    Just (J.Object child) -> Just child
    _ -> Nothing
