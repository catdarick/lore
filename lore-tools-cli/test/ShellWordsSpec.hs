module ShellWordsSpec
  ( spec,
  )
where

import Lore.Tools.Cli.Internal.ShellWords
import Test.Hspec

spec :: Spec
spec =
  describe "shellWords" do
    it "keeps apostrophes inside unquoted Haskell identifiers" do
      shellWords "find-references spec'WholeLogic"
        `shouldBe` ["find-references", "spec'WholeLogic"]

    it "keeps apostrophes in the current completion token" do
      parseLineContext "find-references spec'Whole"
        `shouldBe` LineContext
          { lineWordsBeforeCursor = ["find-references"],
            lineCurrentToken = "spec'Whole",
            lineEndsWithSpace = False,
            lineQuoteMode = QuoteNone
          }

    it "still supports single quoted tokens starting at token boundary" do
      shellWords "find-references 'spec WholeLogic'"
        `shouldBe` ["find-references", "spec WholeLogic"]

    it "still supports double quoted tokens starting at token boundary" do
      shellWords "find-references \"spec WholeLogic\""
        `shouldBe` ["find-references", "spec WholeLogic"]
