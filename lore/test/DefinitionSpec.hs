module DefinitionSpec (spec) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, intercalate, sort)
import Data.Text (pack, unpack)
import qualified GHC
import qualified GHC.Plugins
import Lore.Definition (DeclarationSpans (..), DefinitionSlice (..), declarationSpans, mergeDefinitionSlices, renderDefinitionModulesText, renderImport, requiredImports, resolveDefinitionClosure, resolveDefinitionSlice)
import Lore.Lookup (ExportedSymbol (..), findSymbols)
import Lore.Monad (MonadLore)
import Lore.Targets (defaultLoadTargetsOptions)
import qualified Lore.Targets as Targets
import Test.Hspec
import TestSupport (fixtureLore)

loadTargets :: (MonadLore m) => Targets.LoadTargetsOptions -> m ()
loadTargets options = void (Targets.loadTargets options)

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

    it "resolves imports for definitions that reference another module" do
      slice <- fixtureDefinition "crossModuleRecord"

      shouldHaveSingleDefinitionText
        slice
        "crossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)"
        (Just "crossModuleRecord :: Int -> Support.SupportRecord")
      fmap renderImport slice.requiredImports
        `shouldBe` [ "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportStep)"
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

    it "deduplicates repeated declaration spans when merged slices overlap" do
      zero <- fixtureDefinition "lookupOrZero"

      let merged = mergeDefinitionSlices [zero, zero]

      fmap (length . declarationSpans) merged `shouldBe` Just 1

  describe "resolveDefinitionClosure" do
    it "respects the requested recursion depth for same-module function references" do
      depthZero <- fixtureDefinitionClosure 0 "derivedValue"
      depthOne <- fixtureDefinitionClosure 1 "derivedValue"
      depthTwo <- fixtureDefinitionClosure 2 "derivedValue"

      depthZero
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          ["derivedValue :: Int\nderivedValue = bumpWithSeed 2"],
                                          []
                                        )
                                      ]
      depthOne
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "derivedValue :: Int\nderivedValue = bumpWithSeed 2",
                                            "bumpWithSeed :: Int -> Int\nbumpWithSeed value = value + seedValue"
                                          ],
                                          []
                                        )
                                      ]
      depthTwo
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "derivedValue :: Int\nderivedValue = bumpWithSeed 2",
                                            "bumpWithSeed :: Int -> Int\nbumpWithSeed value = value + seedValue",
                                            "seedValue :: Int\nseedValue = 40"
                                          ],
                                          []
                                        )
                                      ]

    it "includes referenced types when recursively resolving a function definition" do
      closure <- fixtureDefinitionClosure 1 "mkIndexed"

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "mkIndexed :: NameSet -> Indexed Int\nmkIndexed names =\n  Indexed\n    { indexedNames = names,\n      indexedValues = Map.empty\n    }",
                                            "type NameSet = Set.Set String",
                                            "data Indexed a = Indexed\n  { indexedNames :: NameSet,\n    indexedValues :: Map.Map String a\n  }"
                                          ],
                                          [ "import qualified Data.Map.Strict as Map",
                                            "import qualified Data.Set as Set (Set)"
                                          ]
                                        )
                                      ]

    it "stops on already visited definitions when recursion encounters a cycle" do
      closure <- fixtureDefinitionClosure 4 "mutualLeft"

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "mutualLeft :: Int -> Bool\nmutualLeft 0 = True\nmutualLeft n = mutualRight (n - 1)",
                                            "mutualRight :: Int -> Bool\nmutualRight 0 = False\nmutualRight n = mutualLeft (n - 1)"
                                          ],
                                          []
                                        )
                                      ]

    it "recursively resolves referenced symbols across module boundaries" do
      closure <- fixtureDefinitionClosure 2 "crossModuleRecord"

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "crossModuleRecord :: Int -> Support.SupportRecord\ncrossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)"
                                          ],
                                          ["import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportStep)"]
                                        ),
                                        ( "Demo.Support",
                                          [ "data SupportRecord = SupportRecord\n  { supportValues :: Map.Map String Int\n  }",
                                            "mkSupportRecord :: Int -> SupportRecord\nmkSupportRecord value =\n  SupportRecord\n    { supportValues = Map.singleton \"value\" value\n    }",
                                            "supportStep :: Int -> Int\nsupportStep value = value + supportSeed",
                                            "supportSeed :: Int\nsupportSeed = 5"
                                          ],
                                          ["import qualified Data.Map.Strict as Map"]
                                        )
                                      ]

    it "merges qualified explicit import lists when same-module declarations use different items" do
      closure <- fixtureDefinitionClosure 2 "crossModuleBundle"

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "crossModuleBundle :: Int -> (Int, Support.SupportRecord)\ncrossModuleBundle value =\n  (crossModuleSeed, crossModuleRecord value)",
                                            "crossModuleRecord :: Int -> Support.SupportRecord\ncrossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)",
                                            "crossModuleSeed :: Int\ncrossModuleSeed = Support.supportSeed"
                                          ],
                                          ["import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)"]
                                        ),
                                        ( "Demo.Support",
                                          [ "data SupportRecord = SupportRecord\n  { supportValues :: Map.Map String Int\n  }",
                                            "mkSupportRecord :: Int -> SupportRecord\nmkSupportRecord value =\n  SupportRecord\n    { supportValues = Map.singleton \"value\" value\n    }",
                                            "supportStep :: Int -> Int\nsupportStep value = value + supportSeed",
                                            "supportSeed :: Int\nsupportSeed = 5"
                                          ],
                                          ["import qualified Data.Map.Strict as Map"]
                                        )
                                      ]

  describe "renderDefinitionModulesText" do
    it "renders a single definition as a minified module fragment" do
      rendered <- fixtureRenderedDefinition "lookupOrZero"

      rendered
        `shouldBe` intercalate
          "\n"
          [ "=== src/Demo.hs ===",
            "",
            "--- imports ---",
            "import qualified Data.Map.Strict as Map",
            "import Data.Maybe (fromMaybe)",
            "",
            "--- lines 33-35 ---",
            "lookupOrZero :: [(String, Int)] -> String -> Int",
            "lookupOrZero pairs key =",
            "  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
          ]

    it "renders recursive closures grouped by file with reduced imports" do
      rendered <- fixtureRenderedDefinitionClosure 2 "crossModuleRecord"

      rendered
        `shouldBe` intercalate
          "\n"
          [ "=== src/Demo.hs ===",
            "",
            "--- imports ---",
            "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportStep)",
            "",
            "--- lines 60-62 ---",
            "crossModuleRecord :: Int -> Support.SupportRecord",
            "crossModuleRecord value =",
            "  Support.mkSupportRecord (Support.supportStep value)",
            "",
            "=== src/Demo/Support.hs ===",
            "",
            "--- imports ---",
            "import qualified Data.Map.Strict as Map",
            "",
            "--- lines 11-12 ---",
            "supportSeed :: Int",
            "supportSeed = 5",
            "",
            "--- lines 14-15 ---",
            "supportStep :: Int -> Int",
            "supportStep value = value + supportSeed",
            "",
            "--- lines 17-19 ---",
            "data SupportRecord = SupportRecord",
            "  { supportValues :: Map.Map String Int",
            "  }",
            "",
            "--- lines 21-25 ---",
            "mkSupportRecord :: Int -> SupportRecord",
            "mkSupportRecord value =",
            "  SupportRecord",
            "    { supportValues = Map.singleton \"value\" value",
            "    }"
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

shouldHaveModuleDefinitions :: [DefinitionSlice] -> [(String, [String], [String])] -> IO ()
shouldHaveModuleDefinitions slices expectedDefinitions = do
  actualDefinitions <- traverse renderedModuleDefinition slices
  fmap normalizeModuleDefinition actualDefinitions
    `shouldMatchList` fmap normalizeModuleDefinition expectedDefinitions

renderedModuleDefinition :: DefinitionSlice -> IO (String, [String], [String])
renderedModuleDefinition slice = do
  texts <- traverse definitionTextFromSpans slice.declarationSpans
  pure
    ( GHC.moduleNameString (GHC.moduleName slice.definitionModule),
      texts,
      fmap renderImport slice.requiredImports
    )

normalizeModuleDefinition :: (String, [String], [String]) -> (String, [String], [String])
normalizeModuleDefinition (moduleName, definitions, imports) =
  (moduleName, sort definitions, sort imports)

definitionTextFromSpans :: DeclarationSpans -> IO String
definitionTextFromSpans spans = do
  declarationText <- readSpanText spans.declarationSpan
  signatureText <- traverse readSpanText spans.signatureSpan
  pure $
    maybe declarationText (<> "\n" <> declarationText) signatureText

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
    loadTargets defaultLoadTargetsOptions
    exportedSymbols <- findSymbols (pack symbol)
    targetName <- maybe (error ("symbol not found: " <> symbol)) pure (findFixtureSymbol symbol exportedSymbols)
    maybe (error ("definition not found: " <> symbol)) pure =<< resolveDefinitionSlice targetName

fixtureDefinitionClosure :: Int -> String -> IO [DefinitionSlice]
fixtureDefinitionClosure depth symbol =
  fixtureLore do
    loadTargets defaultLoadTargetsOptions
    exportedSymbols <- findSymbols (pack symbol)
    targetName <- maybe (error ("symbol not found: " <> symbol)) pure (findFixtureSymbol symbol exportedSymbols)
    resolveDefinitionClosure depth targetName

fixtureRenderedDefinition :: String -> IO String
fixtureRenderedDefinition symbol =
  fixtureLore do
    loadTargets defaultLoadTargetsOptions
    exportedSymbols <- findSymbols (pack symbol)
    targetName <- maybe (error ("symbol not found: " <> symbol)) pure (findFixtureSymbol symbol exportedSymbols)
    definitionSlice <- maybe (error ("definition not found: " <> symbol)) pure =<< resolveDefinitionSlice targetName
    unpack <$> liftIO (renderDefinitionModulesText [definitionSlice])

fixtureRenderedDefinitionClosure :: Int -> String -> IO String
fixtureRenderedDefinitionClosure depth symbol =
  fixtureLore do
    loadTargets defaultLoadTargetsOptions
    exportedSymbols <- findSymbols (pack symbol)
    targetName <- maybe (error ("symbol not found: " <> symbol)) pure (findFixtureSymbol symbol exportedSymbols)
    definitionClosure <- resolveDefinitionClosure depth targetName
    unpack <$> liftIO (renderDefinitionModulesText definitionClosure)

findFixtureSymbol :: String -> [ExportedSymbol] -> Maybe GHC.Name
findFixtureSymbol symbol =
  fmap name
    . find
      ( \exportedSymbol ->
          GHC.Plugins.getOccString exportedSymbol.name == symbol
            && maybe False ((== "Demo") . GHC.moduleNameString . GHC.moduleName) (GHC.Plugins.nameModule_maybe exportedSymbol.name)
      )
