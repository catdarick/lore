module LookupInstancesSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Mcp.Tools.LookupInstances (lookupInstancesTool)
import McpTestSupport
  ( callToolWithArgs,
    fixtureLoreMcpAtWithCache,
    loadFixtureHomeModules,
    withFixtureCopy,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  describe "lookupInstances" do
    it "renders owner-qualified disambiguation hints for same-module duplicate record fields" do
      result <-
        renderLookupInstancesFixture
          "RenderedDuplicateFieldNames"
          duplicateFieldNamesModuleSource
          ["TestRefs.RenderedDuplicateFieldNames.fieldOne", "Int"]

      result `shouldContainText` "is ambiguous. More qualification is required"
      result `shouldContainText` "TestRefs.RenderedDuplicateFieldNames.fieldOne@RecordOne"
      result `shouldContainText` "TestRefs.RenderedDuplicateFieldNames.fieldOne@RecordTwo"

    it "accepts owner-qualified names to resolve same-module duplicate record fields" do
      result <-
        renderLookupInstancesFixture
          "RenderedDuplicateFieldNames"
          duplicateFieldNamesModuleSource
          ["TestRefs.RenderedDuplicateFieldNames.fieldOne@RecordOne", "Int"]

      result `shouldContainText` "Found 0 matching instances for [\"TestRefs.RenderedDuplicateFieldNames.fieldOne@RecordOne\", \"Int\"]."
      result `shouldNotContainText` "is ambiguous. More qualification is required"

renderLookupInstancesFixture :: FilePath -> Text -> [Text] -> IO Text
renderLookupInstancesFixture moduleFileName moduleSource names =
  withFixtureCopy \fixtureRoot -> do
    let moduleDir = fixtureRoot </> "src" </> "TestRefs"
        moduleFile = moduleDir </> moduleFileName <> ".hs"
    createDirectoryIfMissing True moduleDir
    TIO.writeFile moduleFile moduleSource

    fixtureLoreMcpAtWithCache False fixtureRoot do
      loadFixtureHomeModules
      callToolWithArgs lookupInstancesTool (lookupInstancesArgs names)

lookupInstancesArgs :: [Text] -> J.Value
lookupInstancesArgs names =
  J.object
    [ "names" J..= names
    ]

duplicateFieldNamesModuleSource :: Text
duplicateFieldNamesModuleSource =
  T.unlines
    [ "{-# LANGUAGE DuplicateRecordFields #-}",
      "",
      "module TestRefs.RenderedDuplicateFieldNames",
      "  ( RecordOne(..),",
      "    RecordTwo(..),",
      "    mkRecordOne,",
      "    mkRecordTwo",
      "  ) where",
      "",
      "data RecordOne = RecordOne",
      "  { fieldOne :: !Int,",
      "    fieldTwo :: !Int",
      "    }",
      "",
      "data RecordTwo = RecordTwo",
      "  { fieldOne :: !Int,",
      "    fieldTwo :: !Int",
      "    }",
      "",
      "mkRecordOne :: Int -> RecordOne",
      "mkRecordOne value =",
      "  RecordOne {fieldOne = value, fieldTwo = value + 1}",
      "",
      "mkRecordTwo :: Int -> RecordTwo",
      "mkRecordTwo value =",
      "  RecordTwo {fieldOne = value, fieldTwo = value + 2}"
    ]

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
  T.unpack actual `shouldContain` T.unpack expected

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText actual expected =
  T.unpack actual `shouldNotContain` T.unpack expected
