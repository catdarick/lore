module ImportNormalizeSpec (spec) where

import qualified Data.Text
import Lore.Refactor.Imports
  ( ImportId (..),
    ImportItem (..),
    ImportList (..),
    ImportOperation (..),
    NormalizedImport (..),
    QualifiedImportStyle (..),
    applyImportOperations,
    renderNormalizedImport,
  )
import Test.Hspec

spec :: Spec
spec =
  describe "import normalization" do
    it "removes a whole import" do
      let (normalized, _) =
            applyImportOperations
              [openImport 1 0 "Data.List"]
              [RemoveWholeImport (ImportId 1)]

      normalized `shouldBe` []

    it "removes an item from an explicit import list" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Data.List" [item "find", item "nub"]]
              [RemoveImportItem (ImportId 1) "nub"]

      normalized `shouldBe` [explicitImport 1 0 "Data.List" [item "find"]]

    it "removes an import when its last explicit binding is removed" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Data.List" [item "find"]]
              [RemoveImportItem (ImportId 1) "find"]

      normalized `shouldBe` []

    it "removes a child from a parent import item" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Demo.Types" [item "Foo(A, B)"]]
              [RemoveImportItem (ImportId 1) "B"]

      normalized `shouldBe` [explicitImport 1 0 "Demo.Types" [item "Foo(A)"]]

    it "removes a parent import item when all children are removed" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Demo.Types" [item "Foo(A)"]]
              [RemoveImportItem (ImportId 1) "A"]

      normalized `shouldBe` []

    it "normalizes operator names when removing items" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Data.List" [item "(+)"]]
              [RemoveImportItem (ImportId 1) "+"]

      normalized `shouldBe` []

    it "normalizes pattern imports when removing items" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Demo.Patterns" [item "pattern Foo"]]
              [RemoveImportItem (ImportId 1) "Foo"]

      normalized `shouldBe` []

    it "renders package-qualified imports with quotes" do
      renderNormalizedImport
        ( (baseImport 1 0 "Data.Map.Strict")
            { normalizedImportQualifiedStyle = ImportQualifiedPrefix,
              normalizedImportAlias = Just (fromString "Map"),
              normalizedImportPackageQualifier = Just (fromString "containers")
            }
        )
        `shouldBe` "import \"containers\" qualified Data.Map.Strict as Map"

item :: String -> ImportItem
item text =
  ImportItem
    { importItemText = fromString text,
      importItemSpan = Nothing
    }

explicitImport :: Int -> Int -> String -> [ImportItem] -> NormalizedImport
explicitImport importId order moduleName items =
  (baseImport importId order moduleName)
    { normalizedImportList = ExplicitImport items
    }

openImport :: Int -> Int -> String -> NormalizedImport
openImport importId order moduleName =
  (baseImport importId order moduleName)
    { normalizedImportList = OpenImport
    }

baseImport :: Int -> Int -> String -> NormalizedImport
baseImport importId order moduleName =
  NormalizedImport
    { normalizedImportId = Just (ImportId importId),
      normalizedImportOrder = order,
      normalizedImportSpan = Nothing,
      normalizedImportModuleName = fromString moduleName,
      normalizedImportQualifiedStyle = ImportUnqualified,
      normalizedImportAlias = Nothing,
      normalizedImportSource = False,
      normalizedImportSafe = False,
      normalizedImportPackageQualifier = Nothing,
      normalizedImportList = OpenImport
    }

fromString :: String -> Data.Text.Text
fromString =
  Data.Text.pack
