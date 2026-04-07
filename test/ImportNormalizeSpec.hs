module ImportNormalizeSpec (spec) where

import qualified Data.Text
import Internal.AutoRefact.ImportDecl
  ( ImportId (..),
    ImportItem (..),
    ImportList (..),
    NormalizedImport (..),
    QualifiedImportStyle (..),
  )
import Internal.AutoRefact.ImportNormalize (applyImportOperations, normalizeImports)
import Internal.AutoRefact.ImportOps (ImportOperation (..))
import Test.Hspec

spec :: Spec
spec =
  describe "import normalization" do
    it "merges duplicate explicit imports and deduplicates items" do
      let (normalized, _) =
            normalizeImports
              ( [ explicitImport 1 0 "Data.List" [item "find", item "nubBy"],
                  explicitImport 2 1 "Data.List" [item "find", item "sortOn"]
                ],
                []
              )

      normalized `shouldBe` [explicitImport 1 0 "Data.List" [item "find", item "nubBy", item "sortOn"]]

    it "prefers an open import over explicit imports from the same module" do
      let (normalized, _) =
            normalizeImports
              ( [ explicitImport 1 0 "Data.Text" [item "Text"],
                  openQualifiedImport 2 1 ImportUnqualified "Data.Text" Nothing
                ],
                []
              )

      normalized `shouldBe` [openQualifiedImport 1 0 ImportUnqualified "Data.Text" Nothing]

    it "preserves alias distinctions when normalizing" do
      let (normalized, _) =
            normalizeImports
              ( [ openQualifiedImport 1 0 ImportQualifiedPrefix "Data.Text" (Just "T"),
                  openQualifiedImport 2 1 ImportQualifiedPrefix "Data.Text" (Just "Text")
                ],
                []
              )

      length normalized `shouldBe` 2

    it "preserves prefix and postfix qualified styles separately" do
      let (normalized, _) =
            normalizeImports
              ( [ openQualifiedImport 1 0 ImportQualifiedPrefix "Data.Text" (Just "T"),
                  openQualifiedImport 2 1 ImportQualifiedPostfix "Data.Text" (Just "T")
                ],
                []
              )

      map normalizedImportQualifiedStyle normalized `shouldBe` [ImportQualifiedPrefix, ImportQualifiedPostfix]

    it "opens an existing explicit qualified import instead of inserting another one" do
      let (normalized, _) =
            applyImportOperations
              [qualifiedExplicitImport 1 0 "Data.Set" "Set" [item "Set"]]
              [EnsureQualifiedImport "Data.Set" "Set"]

      normalized `shouldBe` [openQualifiedImport 1 0 ImportQualifiedPrefix "Data.Set" (Just "Set")]

    it "removes an import when its last explicit binding becomes redundant" do
      let (normalized, _) =
            applyImportOperations
              [explicitImport 1 0 "Data.List" [item "find"]]
              [RemoveImportItem (ImportId 1) "find"]

      normalized `shouldBe` []

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

qualifiedExplicitImport :: Int -> Int -> String -> String -> [ImportItem] -> NormalizedImport
qualifiedExplicitImport importId order moduleName qualifier items =
  (explicitImport importId order moduleName items)
    { normalizedImportQualifiedStyle = ImportQualifiedPrefix,
      normalizedImportAlias = Just (fromString qualifier)
    }

openQualifiedImport :: Int -> Int -> QualifiedImportStyle -> String -> Maybe String -> NormalizedImport
openQualifiedImport importId order qualifiedStyle moduleName qualifier =
  (baseImport importId order moduleName)
    { normalizedImportQualifiedStyle = qualifiedStyle,
      normalizedImportAlias = fromString <$> qualifier,
      normalizedImportList = OpenImport
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
