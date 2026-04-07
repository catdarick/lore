module DefinitionSpec (spec) where

import Data.List (find)
import Data.Text (pack)
import qualified GHC
import qualified GHC.Plugins as GHC.Plugins
import Internal.Definition (DeclarationSpans (..), DefinitionSlice (..), declarationSpans, mergeDefinitionSlices, renderImport, requiredImports, resolveDefinitionSlice)
import Internal.Lookup (findSymbol)
import Internal.Lookup.Types (ExportedSymbol (..))
import Internal.Targets (updateTargets)
import Test.Hspec
import TestSupport (fixtureLore)

spec :: Spec
spec = do
  describe "resolveDefinitionSlice" do
    it "resolves declaration spans and the minimal imports for a symbol" do
      slice <- fixtureDefinition "lookupOrZero"

      shouldHaveSingleDefinitionText
        slice
        "lookupOrZero pairs key =\n  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
        (Just "lookupOrZero :: [(String, Int)] -> String -> Int")
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Data.Map.Strict as Map",
                     "import Data.Maybe (fromMaybe)"
                   ]

    it "preserves an explicit list on a qualified aliased import when it existed in source" do
      slice <- fixtureDefinition "explicitQualified"

      shouldHaveSingleDefinitionText
        slice
        "explicitQualified ch =\n  Set.member ch (Set.fromList \"abc\")"
        (Just "explicitQualified :: Char -> Bool")
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Data.Set as Set (fromList, member)"
                   ]

    it "includes references used inside a where block" do
      slice <- fixtureDefinition "lookupWithWhere"

      shouldHaveSingleDefinitionText
        slice
        "lookupWithWhere pairs key =\n  fromMaybe fallback (Map.lookup key table)\n  where\n    table = Map.fromList pairs\n    fallback = Map.size table"
        (Just "lookupWithWhere :: [(String, Int)] -> String -> Int")
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Data.Map.Strict as Map",
                     "import Data.Maybe (fromMaybe)"
                   ]

    it "resolves all clauses of a multi-equation top-level function" do
      slice <- fixtureDefinition "isTrue"

      shouldHaveSingleDefinitionText
        slice
        "isTrue \"True\" = True\nisTrue \"False\" = False\nisTrue _ = False"
        (Just "isTrue :: String -> Bool")
      fmap renderImport slice.requiredImports `shouldBe` []

    it "does not synthesize an explicit Prelude import" do
      slice <- fixtureDefinition "lookupOrZero"

      fmap renderImport slice.requiredImports `shouldSatisfy` all (/= "import Prelude")

    it "resolves the correct declaration for a type alias" do
      slice <- fixtureDefinition "NameSet"

      shouldHaveSingleDefinitionText
        slice
        "type NameSet = Set.Set String"
        Nothing
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Data.Set as Set (Set)"
                   ]

    it "resolves the correct declaration for a type family" do
      slice <- fixtureDefinition "Elem"

      shouldHaveSingleDefinitionText
        slice
        "type family Elem (container :: Type) :: Type"
        Nothing
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import Data.Kind (Type)"
                   ]

    it "resolves the correct declaration for a data family" do
      slice <- fixtureDefinition "Bucket"

      shouldHaveSingleDefinitionText
        slice
        "data family Bucket (item :: Type) :: Type"
        Nothing
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import Data.Kind (Type)"
                   ]

    it "resolves the correct declaration for a data type" do
      slice <- fixtureDefinition "Indexed"

      shouldHaveSingleDefinitionText
        slice
        "data Indexed a = Indexed\n  { indexedNames :: NameSet,\n    indexedValues :: Map.Map String a\n  }"
        Nothing
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Data.Map.Strict as Map"
                   ]

    it "resolves the correct declaration for a class" do
      slice <- fixtureDefinition "HasIndex"

      shouldHaveSingleDefinitionText
        slice
        "class HasIndex a where\n  toIndex :: a -> Map.Map String a"
        Nothing
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Data.Map.Strict as Map"
                   ]

    it "resolves a shared top-level pattern binding for the first bound name" do
      slice <- fixtureDefinition "pairLeft"

      shouldHaveSingleDefinitionText
        slice
        "(pairLeft, pairRight) =\n  ( fromMaybe 0 (Map.lookup \"left\" table),\n    Map.size table\n  )\n  where\n    table = Map.fromList [(\"left\", 1), (\"right\", 2)]"
        (Just "pairLeft, pairRight :: Int")
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Data.Map.Strict as Map",
                     "import Data.Maybe (fromMaybe)"
                   ]

    it "resolves a shared top-level pattern binding for the second bound name" do
      slice <- fixtureDefinition "pairRight"

      shouldHaveSingleDefinitionText
        slice
        "(pairLeft, pairRight) =\n  ( fromMaybe 0 (Map.lookup \"left\" table),\n    Map.size table\n  )\n  where\n    table = Map.fromList [(\"left\", 1), (\"right\", 2)]"
        (Just "pairLeft, pairRight :: Int")
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Data.Map.Strict as Map",
                     "import Data.Maybe (fromMaybe)"
                   ]

  describe "mergeDefinitionSlices" do
    it "merges declarations from the same module and deduplicates imports" do
      zero <- fixtureDefinition "lookupOrZero"
      one <- fixtureDefinition "lookupOrOne"

      let merged = mergeDefinitionSlices [zero, one]

      fmap (length . declarationSpans) merged `shouldBe` Just 2
      fmap (map renderImport . requiredImports) merged
        `shouldBe` Just
          [ "import qualified Data.Map.Strict as Map",
            "import Data.Maybe (fromMaybe)"
          ]

shouldHaveSingleDefinitionText ::
  DefinitionSlice ->
  String ->
  Maybe String ->
  IO ()
shouldHaveSingleDefinitionText slice expectedDeclaration expectedSignature = do
  length slice.declarationSpans `shouldBe` 1
  declarationText <- readSpanText spans.declarationSpan
  signatureText <- traverse readSpanText spans.signatureSpan
  declarationText `shouldBe` expectedDeclaration
  signatureText `shouldBe` expectedSignature
  where
    spans = head slice.declarationSpans

readSpanText :: GHC.SrcSpan -> IO String
readSpanText = \case
  GHC.RealSrcSpan realSpan _ ->
    sliceRealSpan realSpan <$> readFile (GHC.Plugins.unpackFS (GHC.srcSpanFile realSpan))
  other ->
    error ("expected RealSrcSpan, got: " <> show other)

sliceRealSpan :: GHC.RealSrcSpan -> String -> String
sliceRealSpan realSpan contents =
  case drop (GHC.srcSpanStartLine realSpan - 1) (lines contents) of
    [] ->
      ""
    relevantLines ->
      joinLines $
        zipWith sliceLine [GHC.srcSpanStartLine realSpan .. GHC.srcSpanEndLine realSpan] $
          take (GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan + 1) relevantLines
  where
    sliceLine lineNo line
      | lineNo == GHC.srcSpanStartLine realSpan && lineNo == GHC.srcSpanEndLine realSpan =
          take width (drop startCol line)
      | lineNo == GHC.srcSpanStartLine realSpan =
          drop startCol line
      | lineNo == GHC.srcSpanEndLine realSpan =
          take endCol line
      | otherwise =
          line
      where
        startCol = GHC.srcSpanStartCol realSpan - 1
        endCol = GHC.srcSpanEndCol realSpan - 1
        width = endCol - startCol

    joinLines [] = ""
    joinLines xs = foldr1 (\line rest -> line <> "\n" <> rest) xs

fixtureDefinition :: String -> IO DefinitionSlice
fixtureDefinition symbol =
  fixtureLore do
    updateTargets
    exportedSymbols <- findSymbol (pack symbol)
    targetName <- maybe (error ("symbol not found: " <> symbol)) pure (findFixtureSymbol symbol exportedSymbols)
    maybe (error ("definition not found: " <> symbol)) pure =<< resolveDefinitionSlice targetName

findFixtureSymbol :: String -> [ExportedSymbol] -> Maybe GHC.Name
findFixtureSymbol symbol =
  fmap name
    . find
      ( \exportedSymbol ->
          GHC.Plugins.getOccString exportedSymbol.name == symbol
            && maybe False ((== "Demo") . GHC.moduleNameString . GHC.moduleName) (GHC.Plugins.nameModule_maybe exportedSymbol.name)
      )
