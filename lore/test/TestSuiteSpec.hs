module TestSuiteSpec
  ( spec,
  )
where

import Lore.Internal.TestSuite
  ( RunTestSuiteOptions (..),
    effectiveTestArguments,
    parseTestArguments,
  )
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec =
  describe "test suite arguments" do
    it "produces no effective arguments without defaults or explicit arguments" do
      effectiveTestArguments [] (optionsWithExplicitArguments [])
        `shouldBe` []

    it "uses defaults without explicit arguments" do
      effectiveTestArguments ["--default", "one"] (optionsWithExplicitArguments [])
        `shouldBe` ["--default", "one"]

    it "uses explicit arguments without defaults" do
      effectiveTestArguments [] (optionsWithExplicitArguments ["--explicit", "two"])
        `shouldBe` ["--explicit", "two"]

    it "puts defaults before explicit arguments" do
      effectiveTestArguments ["--default", "one"] (optionsWithExplicitArguments ["--explicit", "two"])
        `shouldBe` ["--default", "one", "--explicit", "two"]

    it "parses default and explicit strings independently before merging" do
      let defaultArguments = parseTestArgumentsOrFail "'default value'"
          explicitArguments = parseTestArgumentsOrFail "'explicit value'"
      effectiveTestArguments defaultArguments (optionsWithExplicitArguments explicitArguments)
        `shouldBe` ["default value", "explicit value"]

optionsWithExplicitArguments :: [String] -> RunTestSuiteOptions
optionsWithExplicitArguments arguments =
  RunTestSuiteOptions
    { packageName = Nothing,
      testArguments = arguments
    }

parseTestArgumentsOrFail :: String -> [String]
parseTestArgumentsOrFail raw =
  case parseTestArguments (T.pack raw) of
    Left err ->
      error ("Expected valid test arguments, got: " <> show err)
    Right arguments ->
      arguments
