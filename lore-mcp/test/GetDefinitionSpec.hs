module GetDefinitionSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Mcp.Tools.GetDefinition.Cached (cachedGetDefinitionTool)
import Lore.Mcp.Tools.GetDefinition.Regular (regularGetDefinitionTool)
import Lore.Mcp.Tools.LookupSymbolInfo (lookupSymbolInfoTool)
import Lore.Mcp.Tools.NotifyKnowledgeReset (notifyKnowledgeResetTool)
import McpTestSupport
  ( callToolWithArgs,
    callToolWithoutArgs,
    fixtureLoreMcp,
    fixtureLoreMcpAtWithCache,
    fixtureLoreMcpWithCache,
    loadFixtureTargets,
    withFixtureCopy,
  )
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "getDefinition (cached mode)" do
    it "omits already returned definitions and force=true returns them again" do
      (firstCall, secondCall, forcedCall) <-
        fixtureLoreMcpWithCache True do
          loadFixtureTargets
          firstCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 Nothing)
          secondCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 Nothing)
          forcedCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 (Just True))
          pure (firstCall, secondCall, forcedCall)

      firstCall `shouldContainText` "lookupOrZero"
      secondCall `shouldContainText` "already returned earlier in this MCP session"
      secondCall `shouldContainText` "Demo: lookupOrZero"
      forcedCall `shouldContainText` "lookupOrZero"

    it "remembers recursively returned definitions per symbol and omits them in later direct requests" do
      (recursiveCall, directCall) <-
        fixtureLoreMcpWithCache True do
          loadFixtureTargets
          recursiveCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["derivedValue"] 2 Nothing)
          directCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["bumpWithSeed"] 0 Nothing)
          pure (recursiveCall, directCall)

      recursiveCall `shouldContainText` "bumpWithSeed :: Int -> Int"
      directCall `shouldContainText` "already returned earlier in this MCP session"
      directCall `shouldContainText` "Demo: bumpWithSeed"

    it "returns previously omitted definitions after notifyKnowledgeReset" do
      (cachedCall, resetCall, afterResetCall) <-
        fixtureLoreMcpWithCache True do
          loadFixtureTargets
          _ <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrOne"] 0 Nothing)
          cachedCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrOne"] 0 Nothing)
          resetCall <- callToolWithoutArgs notifyKnowledgeResetTool
          afterResetCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrOne"] 0 Nothing)
          pure (cachedCall, resetCall, afterResetCall)

      cachedCall `shouldContainText` "already returned earlier in this MCP session"
      resetCall `shouldContainText` "Knowledge reset acknowledged. Cleared "
      afterResetCall `shouldContainText` "lookupOrOne"

  describe "getDefinition (regular mode)" do
    it "does not suppress repeated definitions across calls" do
      (firstCall, secondCall) <-
        fixtureLoreMcp do
          loadFixtureTargets
          firstCall <- callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 Nothing)
          secondCall <- callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 Nothing)
          pure (firstCall, secondCall)

      firstCall `shouldContainText` "lookupOrZero"
      secondCall `shouldContainText` "lookupOrZero"
      secondCall `shouldNotContainText` "already returned earlier in this MCP session"

    it "returns all same-module matches for a duplicated qualified symbol name (regression for resolveRequestedSymbols)" do
      withFixtureCopy \fixtureRoot -> do
        addSameModuleDuplicateSymbolFixture fixtureRoot
        (lookupResult, definitionResult) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            lookupResult <- callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "Demo.AmbiguousId")
            definitionResult <- callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["Demo.AmbiguousId"] 0 Nothing)
            pure (lookupResult, definitionResult)

        lookupResult `shouldContainText` "Found 2 symbol candidates:"
        definitionResult `shouldContainText` "type AmbiguousId = Int"
        definitionResult `shouldContainText` "data instance AmbiguousField AmbiguousRec AmbiguousId = AmbiguousId"
        definitionResult `shouldNotContainText` "is ambiguous. More qualification is required"

lookupSymbolInfoArgs :: Text -> J.Value
lookupSymbolInfoArgs symbol =
  J.object
    [ "symbol" J..= symbol
    ]

addSameModuleDuplicateSymbolFixture :: FilePath -> IO ()
addSameModuleDuplicateSymbolFixture fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  source <- TIO.readFile demoFile
  TIO.writeFile demoFile (source <> "\n\n" <> sameModuleDuplicateFixtureDeclarations)

sameModuleDuplicateFixtureDeclarations :: Text
sameModuleDuplicateFixtureDeclarations =
  T.unlines
    [ "data AmbiguousRec = AmbiguousRec",
      "",
      "type AmbiguousId = Int",
      "",
      "data family AmbiguousField rec typ",
      "",
      "data instance AmbiguousField AmbiguousRec AmbiguousId = AmbiguousId"
    ]

getDefinitionArgs :: [Text] -> Int -> Maybe Bool -> J.Value
getDefinitionArgs symbols recursionDepth maybeForce =
  J.object $
    [ "symbols" J..= symbols,
      "recursionDepth" J..= recursionDepth
    ]
      <> case maybeForce of
        Nothing -> []
        Just forceValue -> ["force" J..= forceValue]

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle =
  haystack `shouldSatisfy` T.isInfixOf needle

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText haystack needle =
  haystack `shouldSatisfy` (not . T.isInfixOf needle)
