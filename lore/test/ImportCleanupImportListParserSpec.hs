{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportCleanupImportListParserSpec (spec) where

import qualified Data.Text as T
import Lore.Refactor.ImportCleanup.Internal
  ( ImportItem (..),
    ImportItemChildren (..),
    ImportName (..),
    ImportNamespace (..),
    SepItem (..),
    SepList (..),
    WithRange (..),
    parseImportListPayload,
  )
import Test.Hspec

spec :: Spec
spec =
  describe "import cleanup import-list payload parser" do
    it "parses simple item list" do
      fmap itemHeads (parseImportListPayload "A, B, C")
        `shouldBe` Right ["A", "B", "C"]

    it "parses parent-child items" do
      fmap itemChildrenKinds (parseImportListPayload "Bar(A, B), baz")
        `shouldSatisfy` (\case Right [True, False] -> True; _ -> False)

    it "parses SomeException (SomeException)" do
      fmap itemHeads (parseImportListPayload "SomeException (SomeException), handle")
        `shouldBe` Right ["SomeException", "handle"]

    it "parses operator item with children" do
      fmap itemHeads (parseImportListPayload "(:+:)(L, R), baz")
        `shouldBe` Right ["(:+:)", "baz"]

    it "parses namespace prefixes" do
      fmap itemNamespaces (parseImportListPayload "type T, pattern P")
        `shouldBe` Right [Just TypeNamespace, Just PatternNamespace]

    it "parses wildcard children" do
      fmap itemChildrenKinds (parseImportListPayload "Bar(..), baz")
        `shouldSatisfy` (\case Right [False, False] -> True; _ -> False)

    it "parses empty explicit list payload" do
      fmap itemHeads (parseImportListPayload "")
        `shouldBe` Right []

    it "rejects empty child list" do
      parseImportListPayload "Bar()"
        `shouldSatisfy` isLeft

    it "rejects inline comments" do
      parseImportListPayload "A {- comment -}, B"
        `shouldSatisfy` isLeft

    it "rejects line comments" do
      parseImportListPayload "A, -- comment\nB"
        `shouldSatisfy` isLeft

    it "rejects duplicate commas" do
      parseImportListPayload "A,,B"
        `shouldSatisfy` isLeft

itemHeads :: SepList ImportItem -> [String]
itemHeads parsedList =
  [ showName item.importItemHead.wrValue
  | sepItem <- parsedList.sepListItems,
    let item = sepItem.sepItemValue
  ]

itemNamespaces :: SepList ImportItem -> [Maybe ImportNamespace]
itemNamespaces parsedList =
  [ item.importItemNamespace
  | sepItem <- parsedList.sepListItems,
    let item = sepItem.sepItemValue
  ]

itemChildrenKinds :: SepList ImportItem -> [Bool]
itemChildrenKinds parsedList =
  [ case item.importItemChildren of
      ExplicitChildren _ -> True
      _ -> False
  | sepItem <- parsedList.sepListItems,
    let item = sepItem.sepItemValue
  ]

showName :: ImportName -> String
showName =
  T.unpack . unImportName

isLeft :: Either a b -> Bool
isLeft =
  \case
    Left _ -> True
    Right _ -> False
