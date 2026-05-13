module ImportNormalizeSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text
import Lore.Refactor.Imports
  ( ImportId (..),
    ImportItem (..),
    ImportList (..),
    ImportOperation (..),
    ImportRemovalTarget (..),
    NormalizedImport (..),
    QualifiedImportStyle (..),
    applyImportOperations,
    mkFlatRemovalTarget,
    mkNormalizedImportItem,
    mkScopedRemovalTarget,
    mkWholeImportItemTarget,
    renderNormalizedImport,
    unNormalizedImportItem,
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
              [removeItems 1 [flatTarget "nub"]]

      normalized `shouldBe` [explicitImport 1 0 "Data.List" [item "find"]]

    it "removes an import when its last explicit binding is removed" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Data.List" [item "find"]]
              [removeItems 1 [flatTarget "find"]]

      normalized `shouldBe` []

    it "removes a child from a parent import item" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Demo.Types" [item "Foo(A, B)"]]
              [removeItems 1 [flatTarget "B"]]

      normalized `shouldBe` [explicitImport 1 0 "Demo.Types" [item "Foo(A)"]]

    it "removes a parent import item when all children are removed" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Demo.Types" [item "Foo(A)"]]
              [removeItems 1 [flatTarget "A"]]

      normalized `shouldBe` []

    it "removes a whole parent import item via explicit whole-item target" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Demo.Types" [item "Foo(A, B)", item "Bar(C)"]]
              [removeItems 1 [wholeItemTarget "Foo(A, B)"]]

      normalized `shouldBe` [explicitImport 1 0 "Demo.Types" [item "Bar(C)"]]

    it "removes a child from parent-scoped target T(A)" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Demo.Types" [item "Foo(A, B)"]]
              [removeItems 1 [scopedTarget "Foo" "A"]]

      normalized `shouldBe` [explicitImport 1 0 "Demo.Types" [item "Foo(B)"]]

    it "normalizes operator names when removing items" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Data.List" [item "(+)"]]
              [removeItems 1 [flatTarget "+"]]

      normalized `shouldBe` []

    it "normalizes pattern imports when removing items" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Demo.Patterns" [item "pattern Foo"]]
              [removeItems 1 [flatTarget "Foo"]]

      normalized `shouldBe` []

    it "normalizes operator text through public constructors" do
      unNormalizedImportItem (mkNormalizedImportItem "(+)")
        `shouldBe` "+"

    it "normalizes pattern text through public constructors" do
      unNormalizedImportItem (mkNormalizedImportItem "pattern Foo")
        `shouldBe` "Foo"

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

removeItems :: Int -> [ImportRemovalTarget] -> ImportOperation
removeItems importId targets =
  case targets of
    firstTarget : remainingTargets ->
      RemoveImportItems (ImportId importId) (firstTarget :| remainingTargets)
    [] ->
      error "removeItems requires at least one target"

flatTarget :: String -> ImportRemovalTarget
flatTarget binding =
  mkFlatRemovalTarget (fromString binding)

scopedTarget :: String -> String -> ImportRemovalTarget
scopedTarget parent binding =
  mkScopedRemovalTarget (fromString parent) (fromString binding)

wholeItemTarget :: String -> ImportRemovalTarget
wholeItemTarget importItemText =
  mkWholeImportItemTarget (fromString importItemText)
