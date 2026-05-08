module FindReferencesSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Mcp.Tools.FindReferences (findReferencesTool)
import McpTestSupport
  ( callToolWithArgs,
    fixtureLoreMcpAtWithCache,
    loadFixtureTargets,
    withFixtureCopy,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "findReferences" do
    it "renders the full useful context for multiline application references" do
      result <-
        renderFindReferencesFixture
          "RenderedCall"
          multilineApplicationReferenceModuleSource
          "TestRefs.RenderedCall.targetSymbol"

      result `shouldContainText` "build a b c ="
      result `shouldContainText` "  callSomeFunction"
      result `shouldContainText` "    a"
      result `shouldContainText` "    b"
      result `shouldContainText` "    targetSymbol"
      result `shouldContainText` "    c"

    it "renders useful context for do-block references" do
      result <-
        renderFindReferencesFixture
          "RenderedDo"
          doBlockReferenceModuleSource
          "TestRefs.RenderedDo.targetSymbol"

      result `shouldContainText` "build value = do"
      result `shouldContainText` "  y <- pure value"
      result `shouldContainText` "  pure (targetSymbol y)"

    it "renders useful context for case alternative references" do
      result <-
        renderFindReferencesFixture
          "RenderedCase"
          caseAlternativeReferenceModuleSource
          "TestRefs.RenderedCase.targetSymbol"

      result `shouldContainText` "build flag ="
      result `shouldContainText` "  case flag of"
      result `shouldContainText` "    True -> targetSymbol"
      result `shouldContainText` "    False -> 0"

    it "renders signature-only references intentionally" do
      result <-
        renderFindReferencesFixture
          "RenderedSignature"
          signatureReferenceModuleSource
          "TestRefs.RenderedSignature.TargetType"

      result `shouldContainText` "foo :: TargetType -> Int"
      result `shouldContainText` "foo _ = 1"

renderFindReferencesFixture :: FilePath -> Text -> Text -> IO Text
renderFindReferencesFixture moduleFileName moduleSource symbol =
  withFixtureCopy \fixtureRoot -> do
    let moduleDir = fixtureRoot </> "src" </> "TestRefs"
        moduleFile = moduleDir </> moduleFileName <> ".hs"
    createDirectoryIfMissing True moduleDir
    TIO.writeFile moduleFile moduleSource

    fixtureLoreMcpAtWithCache False fixtureRoot do
      loadFixtureTargets
      callToolWithArgs findReferencesTool (findReferencesArgs symbol)

findReferencesArgs :: Text -> J.Value
findReferencesArgs symbol =
  J.object
    [ "symbol" J..= symbol
    ]

multilineApplicationReferenceModuleSource :: Text
multilineApplicationReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedCall",
      "  ( targetSymbol,",
      "    build",
      "  ) where",
      "",
      "targetSymbol :: Int",
      "targetSymbol = 1",
      "",
      "build :: Int -> Int -> Int -> Int",
      "build a b c =",
      "  callSomeFunction",
      "    a",
      "    b",
      "    targetSymbol",
      "    c",
      "",
      "callSomeFunction :: Int -> Int -> Int -> Int -> Int",
      "callSomeFunction a b c d = a + b + c + d"
    ]

doBlockReferenceModuleSource :: Text
doBlockReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedDo",
      "  ( targetSymbol,",
      "    build",
      "  ) where",
      "",
      "targetSymbol :: Int -> Int",
      "targetSymbol value = value + 1",
      "",
      "build :: Int -> IO Int",
      "build value = do",
      "  y <- pure value",
      "  pure (targetSymbol y)"
    ]

caseAlternativeReferenceModuleSource :: Text
caseAlternativeReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedCase",
      "  ( targetSymbol,",
      "    build",
      "  ) where",
      "",
      "targetSymbol :: Int",
      "targetSymbol = 1",
      "",
      "build :: Bool -> Int",
      "build flag =",
      "  case flag of",
      "    True -> targetSymbol",
      "    False -> 0"
    ]

signatureReferenceModuleSource :: Text
signatureReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedSignature",
      "  ( TargetType(..),",
      "    foo",
      "  ) where",
      "",
      "data TargetType = TargetType",
      "",
      "foo :: TargetType -> Int",
      "foo _ = 1"
    ]

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
  T.unpack actual `shouldContain` T.unpack expected
