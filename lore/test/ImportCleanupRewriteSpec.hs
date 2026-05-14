{-# LANGUAGE OverloadedStrings #-}

module ImportCleanupRewriteSpec (spec) where

import Lore.Refactor.ImportCleanup.Internal (ImportCleanupWarning (..), ImportId (..), RedundantImportedOccurrence (..), cleanupImportListPayloadOccurrences)
import Test.Hspec

spec :: Spec
spec =
  describe "import cleanup rewrite (payload-only)" do
    it "removes first item" do
      cleanup "A, B, C" [occ "A"]
        `shouldBe` Right "B, C"

    it "removes middle item" do
      cleanup "A, B, C" [occ "B"]
        `shouldBe` Right "A, C"

    it "removes last item" do
      cleanup "A, B, C" [occ "C"]
        `shouldBe` Right "A, B"

    it "keeps empty payload when removing only item" do
      cleanup "A" [occ "A"]
        `shouldBe` Right ""

    it "removes child from parent item" do
      cleanup "Bar(A, B), baz" [occ "A"]
        `shouldBe` Right "Bar(B), baz"

    it "collapses single child to parent head" do
      cleanup "Bar(A), baz" [occ "A"]
        `shouldBe` Right "Bar, baz"

    it "handles SomeException constructor cleanup" do
      cleanup "SomeException (SomeException), handle" [occ "SomeException"]
        `shouldBe` Right "SomeException, handle"

    it "reparses between sequential removals" do
      cleanup "Bar(A, B), A, C" [occ "B", occ "C"]
        `shouldBe` Right "Bar(A), A"

    it "fails ambiguous matches" do
      cleanup "Bar(A), A" [occ "A"]
        `shouldBe` Left (AmbiguousImportBinding (ImportId 1) "A")
  where
    cleanup payload occurrences =
      cleanupImportListPayloadOccurrences (ImportId 1) payload occurrences

    occ name =
      RedundantImportedOccurrence name Nothing
