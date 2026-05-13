{-# LANGUAGE OverloadedStrings #-}

module SourceEditSpec
  ( spec,
  )
where

import Data.List (isInfixOf)
import qualified Data.Text as T
import Lore.SourceEdit
  ( EditValidationWarning (..),
    FileEdit (..),
    Span (..),
    applyReplacementEditsValidated,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "source edit validation" do
    it "drops all conflicting same-span edits instead of applying one side" do
      let source = "module Demo where\nvalue = 1\n"
          (updated, warnings) =
            applyReplacementEditsValidated
              source
              "Demo.hs"
              [ replace (Span "Demo.hs" 2 9 2 10) "2",
                replace (Span "Demo.hs" 2 9 2 10) "42"
              ]

      updated `shouldBe` source
      show warnings `shouldSatisfy` isInfixOf "ConflictingFileEdits"

    it "drops all edits in an overlapping conflict group and still applies unrelated edits" do
      let source = "abcde\n"
          (updated, warnings) =
            applyReplacementEditsValidated
              source
              "Demo.hs"
              [ replace (Span "Demo.hs" 1 1 1 4) "XXX",
                replace (Span "Demo.hs" 1 3 1 6) "YYY",
                replace (Span "Demo.hs" 1 6 1 6) "!"
              ]

      updated `shouldBe` "abcde!\n"
      show warnings `shouldSatisfy` isInfixOf "ConflictingFileEdits"

    it "deduplicates exact duplicate edits without warnings" do
      let source = "value = 1\n"
          edit = replace (Span "Demo.hs" 1 9 1 10) "2"
          (updated, warnings) =
            applyReplacementEditsValidated source "Demo.hs" [edit, edit]

      updated `shouldBe` "value = 2\n"
      warnings `shouldBe` []

    it "reports invalid spans and skips invalid edits" do
      let source = "x\n"
          badEdit = replace (Span "Demo.hs" 10 1 10 2) "y"
          (updated, warnings) =
            applyReplacementEditsValidated source "Demo.hs" [badEdit]

      updated `shouldBe` source
      warnings `shouldBe` [InvalidFileEditSpan "Demo.hs" badEdit]

replace :: Span -> T.Text -> FileEdit
replace span' replacementText =
  ReplaceSpanEdit "Demo.hs" span' replacementText
