module DefinitionSpec (spec) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.List (find, intercalate, isInfixOf, nub, sort, sortOn)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import qualified GHC.Plugins
import Lore (RootSymbolInfo (..), lookupRootSymbolInfoWithChain)
import Lore.Definition (DeclarationSpans (..), DefinitionSlice (..), ImportQualifiedStyle (..), ReferenceMatch (..), RequiredImport (..), declarationSpans, mergeDefinitionSlices, resolveDefinitionClosure, resolveDefinitionSlice, resolveReferenceMatchesForNames)
import Lore.Lookup (Symbol (..), findSymbols)
import Lore.Monad (MonadLore)
import Lore.Targets (defaultLoadTargetsOptions)
import qualified Lore.Targets as Targets
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, splitDirectories, (</>))
import Test.Hspec
import TestSupport (fixtureLore, fixtureLoreAt, withFixtureCopy)

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
      let imports = slice.requiredImports
      ( any (\i -> GHC.moduleNameString i.importModule == "Data.Map.Strict" && i.importQualifiedStyle == Lore.Definition.QualifiedPre) imports
          && any (\i -> GHC.moduleNameString i.importModule == "Data.Maybe") imports
        )
        `shouldBe` True

    it "preserves an explicit list on a qualified aliased import when it existed in source" do
      slice <- fixtureDefinition "explicitQualified"

      shouldHaveSingleDefinitionText
        slice
        "explicitQualified ch =\n  Set.member ch (Set.fromList \"abc\")"
        (Just "explicitQualified :: Char -> Bool")
      let imports = slice.requiredImports
      any (\i -> GHC.moduleNameString i.importModule == "Data.Set" && not (null i.importItems)) imports
        `shouldBe` True

    it "resolves imports for definitions that reference another module" do
      slice <- fixtureDefinition "crossModuleRecord"

      shouldHaveSingleDefinitionText
        slice
        "crossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)"
        (Just "crossModuleRecord :: Int -> Support.SupportRecord")
      let imports = slice.requiredImports
      any (\i -> GHC.moduleNameString i.importModule == "Demo.Support") imports
        `shouldBe` True

    it "resolves definitions for unexported symbols from another home module" do
      slice <-
        fixtureLore do
          loadTargets defaultLoadTargetsOptions
          symbols <- findSymbols "Demo.Support.supportValues"
          targetName <-
            maybe
              (error "symbol not found: Demo.Support.supportValues")
              pure
              (findFixtureSymbolInModule "Demo.Support" "supportValues" symbols)
          maybe
            (error "definition not found: Demo.Support.supportValues")
            pure
            =<< resolveDefinitionSlice targetName

      definitionTexts <- traverse definitionTextFromSpans slice.declarationSpans
      GHC.moduleNameString (GHC.moduleName slice.definitionModule) `shouldBe` "Demo.Support"
      any (isInfixOf "supportValues :: Map.Map String Int") definitionTexts `shouldBe` True

    it "includes references used inside a where block" do
      slice <- fixtureDefinition "lookupWithWhere"

      shouldHaveSingleDefinitionText
        slice
        "lookupWithWhere pairs key =\n  fromMaybe fallback (Map.lookup key table)\n  where\n    table = Map.fromList pairs\n    fallback = Map.size table"
        (Just "lookupWithWhere :: [(String, Int)] -> String -> Int")
      let imports = slice.requiredImports
      ( any (\i -> GHC.moduleNameString i.importModule == "Data.Map.Strict") imports
          && any (\i -> GHC.moduleNameString i.importModule == "Data.Maybe") imports
        )
        `shouldBe` True

    it "resolves all clauses of a multi-equation top-level function" do
      slice <- fixtureDefinition "isTrue"

      shouldHaveSingleDefinitionText
        slice
        "isTrue \"True\" = True\nisTrue \"False\" = False\nisTrue _ = False"
        (Just "isTrue :: String -> Bool")
      null slice.requiredImports `shouldBe` True

    it "does not synthesize an explicit Prelude import" do
      slice <- fixtureDefinition "lookupOrZero"

      all ((/= "Prelude") . GHC.moduleNameString . Lore.Definition.importModule) slice.requiredImports
        `shouldBe` True

    it "resolves the correct declaration for a type alias" do
      slice <- fixtureDefinition "NameSet"

      shouldHaveSingleDefinitionText
        slice
        "type NameSet = Set.Set String"
        Nothing
      let imports = slice.requiredImports
      any (\i -> GHC.moduleNameString i.importModule == "Data.Set") imports
        `shouldBe` True

    it "resolves the correct declaration for a type family" do
      slice <- fixtureDefinition "Elem"

      shouldHaveSingleDefinitionText
        slice
        "type family Elem (container :: Type) :: Type"
        Nothing
      let imports = slice.requiredImports
      any (\i -> GHC.moduleNameString i.importModule == "Data.Kind") imports
        `shouldBe` True

    it "resolves the correct declaration for a data family" do
      slice <- fixtureDefinition "Bucket"

      shouldHaveSingleDefinitionText
        slice
        "data family Bucket (item :: Type) :: Type"
        Nothing
      let imports = slice.requiredImports
      any (\i -> GHC.moduleNameString i.importModule == "Data.Kind") imports
        `shouldBe` True

    it "resolves the correct declaration for a data type" do
      slice <- fixtureDefinition "Indexed"

      shouldHaveSingleDefinitionText
        slice
        "data Indexed a = Indexed\n  { indexedNames :: NameSet,\n    indexedValues :: Map.Map String a\n  }"
        Nothing
      let imports = slice.requiredImports
      any (\i -> GHC.moduleNameString i.importModule == "Data.Map.Strict") imports
        `shouldBe` True

    it "resolves the correct declaration for a class" do
      slice <- fixtureDefinition "HasIndex"

      shouldHaveSingleDefinitionText
        slice
        "class HasIndex a where\n  toIndex :: a -> Map.Map String a"
        Nothing
      let imports = slice.requiredImports
      any (\i -> GHC.moduleNameString i.importModule == "Data.Map.Strict") imports
        `shouldBe` True

    it "survives two consecutive reloads before resolving a definition slice" do
      slice <-
        fixtureLore do
          loadTargets defaultLoadTargetsOptions
          loadTargets defaultLoadTargetsOptions
          exportedSymbols <- findSymbols "lookupOrZero"
          targetName <- maybe (error "symbol not found: lookupOrZero") pure (findFixtureSymbol "lookupOrZero" exportedSymbols)
          maybe (error "definition not found: lookupOrZero") pure =<< resolveDefinitionSlice targetName

      shouldHaveSingleDefinitionText
        slice
        "lookupOrZero pairs key =\n  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
        (Just "lookupOrZero :: [(String, Int)] -> String -> Int")
      let imports = slice.requiredImports
      ( any (\i -> GHC.moduleNameString i.importModule == "Data.Map.Strict") imports
          && any (\i -> GHC.moduleNameString i.importModule == "Data.Maybe") imports
        )
        `shouldBe` True

    it "resolves a shared top-level pattern binding for the first bound name" do
      slice <- fixtureDefinition "pairLeft"

      shouldHaveSingleDefinitionText
        slice
        "(pairLeft, pairRight) =\n  ( fromMaybe 0 (Map.lookup \"left\" table),\n    Map.size table\n  )\n  where\n    table = Map.fromList [(\"left\", 1), (\"right\", 2)]"
        (Just "pairLeft, pairRight :: Int")
      let imports = slice.requiredImports
      ( any (\i -> GHC.moduleNameString i.importModule == "Data.Map.Strict") imports
          && any (\i -> GHC.moduleNameString i.importModule == "Data.Maybe") imports
        )
        `shouldBe` True

    it "resolves a shared top-level pattern binding for the second bound name" do
      slice <- fixtureDefinition "pairRight"

      shouldHaveSingleDefinitionText
        slice
        "(pairLeft, pairRight) =\n  ( fromMaybe 0 (Map.lookup \"left\" table),\n    Map.size table\n  )\n  where\n    table = Map.fromList [(\"left\", 1), (\"right\", 2)]"
        (Just "pairLeft, pairRight :: Int")
      let imports = slice.requiredImports
      ( any (\i -> GHC.moduleNameString i.importModule == "Data.Map.Strict") imports
          && any (\i -> GHC.moduleNameString i.importModule == "Data.Maybe") imports
        )
        `shouldBe` True

  describe "mergeDefinitionSlices" do
    it "merges declarations from the same module and deduplicates imports" do
      zero <- fixtureDefinition "lookupOrZero"
      one <- fixtureDefinition "lookupOrOne"

      case mergeDefinitionSlices [zero, one] of
        Just merged -> do
          length (declarationSpans merged) `shouldBe` 2
          map (GHC.moduleNameString . Lore.Definition.importModule) (Lore.Definition.requiredImports merged)
            `shouldMatchList` ["Data.Map.Strict", "Data.Maybe"]
        Nothing ->
          expectationFailure "expected merged slice"

    it "deduplicates repeated declaration spans when merged slices overlap" do
      zero <- fixtureDefinition "lookupOrZero"

      case mergeDefinitionSlices [zero, zero] of
        Just merged ->
          length (declarationSpans merged) `shouldBe` 1
        Nothing ->
          expectationFailure "expected merged slice"

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
                                          [ "Data.Map.Strict",
                                            "Data.Set"
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
                                          ["Demo.Support"]
                                        ),
                                        ( "Demo.Support",
                                          [ "data SupportRecord = SupportRecord\n  { supportValues :: Map.Map String Int\n  }",
                                            "mkSupportRecord :: Int -> SupportRecord\nmkSupportRecord value =\n  SupportRecord\n    { supportValues = Map.singleton \"value\" value\n    }",
                                            "supportStep :: Int -> Int\nsupportStep value = value + supportSeed",
                                            "supportSeed :: Int\nsupportSeed = 5"
                                          ],
                                          ["Data.Map.Strict"]
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
                                          ["Demo.Support"]
                                        ),
                                        ( "Demo.Support",
                                          [ "data SupportRecord = SupportRecord\n  { supportValues :: Map.Map String Int\n  }",
                                            "mkSupportRecord :: Int -> SupportRecord\nmkSupportRecord value =\n  SupportRecord\n    { supportValues = Map.singleton \"value\" value\n    }",
                                            "supportStep :: Int -> Int\nsupportStep value = value + supportSeed",
                                            "supportSeed :: Int\nsupportSeed = 5"
                                          ],
                                          ["Data.Map.Strict"]
                                        )
                                      ]

    it "includes the concretely used class instance in recursive closure output" do
      withFixtureCopy \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "Render.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile usedInstanceClosureFixtureModuleSource

        closure <-
          fixtureLoreAt fixtureRoot do
            loadTargets defaultLoadTargetsOptions
            exportedSymbols <- findSymbols "TestClosure.Render.renderInt"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.Render.renderInt")
                pure
                (findFixtureSymbolInModule "TestClosure.Render" "renderInt" exportedSymbols)
            resolveDefinitionClosure 1 targetName

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.Render",
                                            [ "class Render a where\n  render :: a -> String",
                                              "instance Render Int where\n  render value = \"int:\" <> show value",
                                              "renderInt :: Int -> String\nrenderInt = render"
                                            ],
                                            []
                                          )
                                        ]

    it "survives two consecutive reloads before resolving a definition closure" do
      closure <-
        fixtureLore do
          loadTargets defaultLoadTargetsOptions
          loadTargets defaultLoadTargetsOptions
          exportedSymbols <- findSymbols "crossModuleRecord"
          targetName <- maybe (error "symbol not found: crossModuleRecord") pure (findFixtureSymbol "crossModuleRecord" exportedSymbols)
          resolveDefinitionClosure 2 targetName

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "crossModuleRecord :: Int -> Support.SupportRecord\ncrossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)"
                                          ],
                                          ["Demo.Support"]
                                        ),
                                        ( "Demo.Support",
                                          [ "data SupportRecord = SupportRecord\n  { supportValues :: Map.Map String Int\n  }",
                                            "mkSupportRecord :: Int -> SupportRecord\nmkSupportRecord value =\n  SupportRecord\n    { supportValues = Map.singleton \"value\" value\n    }",
                                            "supportStep :: Int -> Int\nsupportStep value = value + supportSeed",
                                            "supportSeed :: Int\nsupportSeed = 5"
                                          ],
                                          ["Data.Map.Strict"]
                                        )
                                      ]

  describe "resolveReferenceDefinitions" do
    it "finds top-level definitions and instance definitions that reference the target" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableFlexibleInstances fixtureRoot
        TIO.appendFile demoFile referenceInstanceDefinitions

        references <-
          fixtureLoreAt fixtureRoot do
            loadTargets defaultLoadTargetsOptions
            exportedSymbols <- findSymbols "Demo.Support.supportSeed"
            targetName <-
              maybe
                (error "symbol not found: Demo.Support.supportSeed")
                pure
                (findFixtureSymbolInModule "Demo.Support" "supportSeed" exportedSymbols)
            resolveReferenceMatchesForNames [targetName]

        let referenceSlices = map referenceSlice references
        referenceSlices
          `shouldHaveModuleDefinitions` [ ( "Demo",
                                            ["crossModuleSeed :: Int\ncrossModuleSeed = Support.supportSeed"],
                                            ["Demo.Support"]
                                          ),
                                          ( "Demo",
                                            ["instance HasIndex Support.SupportRecord where\n  toIndex _ =\n    Map.singleton (show Support.supportSeed) (Support.mkSupportRecord Support.supportSeed)"],
                                            [ "Data.Map.Strict",
                                              "Demo.Support"
                                            ]
                                          ),
                                          ( "Demo.Support",
                                            ["supportStep :: Int -> Int\nsupportStep value = value + supportSeed"],
                                            []
                                          )
                                        ]

    it "merges root chains with the same root before matching references" do
      withFixtureCopy \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestChain"
            moduleFile = moduleDir </> "Roots.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile mergedRootChainFixtureModuleSource

        references <-
          fixtureLoreAt fixtureRoot do
            loadTargets defaultLoadTargetsOptions
            resolvedRoots <- lookupRootSymbolInfoWithChain "TestChain.Roots.Wrapped"
            case resolvedRoots of
              [resolvedRoot] ->
                resolveReferenceMatchesForNames resolvedRoot.rootSymbolChain
              _ ->
                error ("unexpected resolved roots count: " <> show (length resolvedRoots))

        let referenceSlices = map referenceSlice references
        referenceSlices
          `shouldHaveModuleDefinitions` [ ( "TestChain.Roots",
                                            ["mkWrapped :: Int -> Wrapped\nmkWrapped = Wrapped"],
                                            []
                                          ),
                                          ( "TestChain.Roots",
                                            ["unwrapWrapped :: Wrapped -> Int\nunwrapWrapped (Wrapped value) = value"],
                                            []
                                          )
                                        ]

  describe "resolveReferenceMatches" do
    it "returns exact matched reference spans for each occurrence in a definition" do
      withFixtureCopy \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestRefs"
            moduleFile = moduleDir </> "Snippet.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile preciseReferenceMatchesFixtureModuleSource

        referenceMatches <-
          fixtureLoreAt fixtureRoot do
            loadTargets defaultLoadTargetsOptions
            exportedSymbols <- findSymbols "TestRefs.Snippet.target"
            targetName <-
              maybe
                (error "symbol not found: TestRefs.Snippet.target")
                pure
                (findFixtureSymbolInModule "TestRefs.Snippet" "target" exportedSymbols)
            resolveReferenceMatchesForNames [targetName]

        case referenceMatches of
          [ReferenceMatch {referenceSlice, matchedReferenceSpans, matchedReferenceUsageSpans}] -> do
            GHC.moduleNameString (GHC.moduleName referenceSlice.definitionModule) `shouldBe` "TestRefs.Snippet"
            sort (mapMaybe matchedSpanStartLine matchedReferenceSpans) `shouldBe` [11, 12, 13]
            mapMaybe matchedSpanLineRange matchedReferenceUsageSpans `shouldSatisfy` elem (11, 13)
          other ->
            expectationFailure ("expected a single reference match, got: " <> show (length other))

    it "collects multiline usage spans for record constructor references" do
      withFixtureCopy \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestRefs"
            moduleFile = moduleDir </> "RecordSnippet.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile multilineRecordReferenceFixtureModuleSource

        referenceMatches <-
          fixtureLoreAt fixtureRoot do
            loadTargets defaultLoadTargetsOptions
            resolvedRoots <- lookupRootSymbolInfoWithChain "TestRefs.RecordSnippet.Result"
            case resolvedRoots of
              [resolvedRoot] ->
                resolveReferenceMatchesForNames resolvedRoot.rootSymbolChain
              _ ->
                error ("unexpected resolved roots count: " <> show (length resolvedRoots))

        case referenceMatches of
          [ReferenceMatch {matchedReferenceSpans, matchedReferenceUsageSpans}] -> do
            mapMaybe matchedSpanStartLine matchedReferenceSpans `shouldSatisfy` elem 10
            mapMaybe matchedSpanLineRange matchedReferenceUsageSpans `shouldBe` [(10, 17)]
          other ->
            expectationFailure ("expected a single reference match, got: " <> show (length other))

    it "collects multiline usage spans for multiline function applications" do
      withFixtureCopy \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestRefs"
            moduleFile = moduleDir </> "CallSnippet.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile multilineFunctionReferenceFixtureModuleSource

        referenceMatches <-
          fixtureLoreAt fixtureRoot do
            loadTargets defaultLoadTargetsOptions
            exportedSymbols <- findSymbols "TestRefs.CallSnippet.target"
            targetName <-
              maybe
                (error "symbol not found: TestRefs.CallSnippet.target")
                pure
                (findFixtureSymbolInModule "TestRefs.CallSnippet" "target" exportedSymbols)
            resolveReferenceMatchesForNames [targetName]

        case referenceMatches of
          [ReferenceMatch {matchedReferenceSpans, matchedReferenceUsageSpans}] -> do
            sort (mapMaybe matchedSpanStartLine matchedReferenceSpans) `shouldBe` [11]
            mapMaybe matchedSpanLineRange matchedReferenceUsageSpans
              `shouldSatisfy` any (\(startLine, endLine) -> startLine == 11 && endLine > startLine)
          other ->
            expectationFailure ("expected a single reference match, got: " <> show (length other))

    it "keeps multiple parent usage spans for nested multiline references" do
      withFixtureCopy \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestRefs"
            moduleFile = moduleDir </> "NestedSnippet.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile nestedMultilineReferenceFixtureModuleSource

        referenceMatches <-
          fixtureLoreAt fixtureRoot do
            loadTargets defaultLoadTargetsOptions
            exportedSymbols <- findSymbols "TestRefs.NestedSnippet.target"
            targetName <-
              maybe
                (error "symbol not found: TestRefs.NestedSnippet.target")
                pure
                (findFixtureSymbolInModule "TestRefs.NestedSnippet" "target" exportedSymbols)
            resolveReferenceMatchesForNames [targetName]

        case referenceMatches of
          [ReferenceMatch {matchedReferenceSpans, matchedReferenceUsageSpans}] -> do
            sort (mapMaybe matchedSpanStartLine matchedReferenceSpans) `shouldBe` [13]
            let usageRanges = sort (nub (mapMaybe matchedSpanLineRange matchedReferenceUsageSpans))
            usageRanges `shouldSatisfy` elem (12, 15)
            usageRanges `shouldSatisfy` elem (11, 15)
          other ->
            expectationFailure ("expected a single reference match, got: " <> show (length other))

    it "collects AST section spans for case alternative references" do
      withFixtureCopy \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestRefs"
            moduleFile = moduleDir </> "CaseSectionSnippet.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile caseSectionReferenceFixtureModuleSource

        referenceMatches <-
          fixtureLoreAt fixtureRoot do
            loadTargets defaultLoadTargetsOptions
            exportedSymbols <- findSymbols "TestRefs.CaseSectionSnippet.target"
            targetName <-
              maybe
                (error "symbol not found: TestRefs.CaseSectionSnippet.target")
                pure
                (findFixtureSymbolInModule "TestRefs.CaseSectionSnippet" "target" exportedSymbols)
            resolveReferenceMatchesForNames [targetName]

        case referenceMatches of
          [ReferenceMatch {matchedReferenceSpans, matchedReferenceSectionSpans}] -> do
            sort (mapMaybe matchedSpanStartLine matchedReferenceSpans) `shouldBe` [14]
            sort (nub (mapMaybe matchedSpanLineRange matchedReferenceSectionSpans)) `shouldSatisfy` elem (13, 15)
          other ->
            expectationFailure ("expected a single reference match, got: " <> show (length other))

  describe "renderDefinitionModulesText" do
    it "renders a single definition as a minified module fragment" do
      rendered <- fixtureRenderedDefinition "lookupOrZero"

      rendered
        `shouldBe` unlines
          [ "=== src/Demo.hs ===",
            "--- lines 33-35 ---",
            "lookupOrZero :: [(String, Int)] -> String -> Int",
            "lookupOrZero pairs key =",
            "  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
          ]

    it "renders recursive closures grouped by file with reduced imports" do
      rendered <- fixtureRenderedDefinitionClosure 2 "crossModuleRecord"

      rendered
        `shouldBe` unlines
          [ "=== src/Demo/Support.hs ===",
            "--- lines 12-13 ---",
            "supportSeed :: Int",
            "supportSeed = 5",
            "--- lines 15-16 ---",
            "supportStep :: Int -> Int",
            "supportStep value = value + supportSeed",
            "--- lines 18-20 ---",
            "data SupportRecord = SupportRecord",
            "  { supportValues :: Map.Map String Int",
            "  }",
            "--- lines 22-26 ---",
            "mkSupportRecord :: Int -> SupportRecord",
            "mkSupportRecord value =",
            "  SupportRecord",
            "    { supportValues = Map.singleton \"value\" value",
            "    }",
            "",
            "=== src/Demo.hs ===",
            "--- lines 60-62 ---",
            "crossModuleRecord :: Int -> Support.SupportRecord",
            "crossModuleRecord value =",
            "  Support.mkSupportRecord (Support.supportStep value)"
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
      fmap (GHC.moduleNameString . Lore.Definition.importModule) slice.requiredImports
    )

normalizeModuleDefinition :: (String, [String], [String]) -> (String, [String], [String])
normalizeModuleDefinition (moduleName, definitions, imports) =
  (moduleName, sort definitions, sort (nubOrd imports))

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
  renderDefinitionSlicesText . pure =<< fixtureDefinition symbol

fixtureRenderedDefinitionClosure :: Int -> String -> IO String
fixtureRenderedDefinitionClosure depth symbol =
  renderDefinitionSlicesText =<< fixtureDefinitionClosure depth symbol

renderDefinitionSlicesText :: [DefinitionSlice] -> IO String
renderDefinitionSlicesText definitionSlices = do
  renderedModules <- traverse renderDefinitionModuleFragment (mergeRenderedDefinitionModules definitionSlices)
  pure . unlines $ intercalate [""] (map lines renderedModules)

renderDefinitionModuleFragment :: DefinitionSlice -> IO String
renderDefinitionModuleFragment definitionSlice = do
  renderedPath <- renderDefinitionModulePath definitionSlice
  renderedDeclarations <- traverse renderDefinitionBlock (sortDeclarationSpans definitionSlice.declarationSpans)
  pure . unlines $ ["=== " <> renderedPath <> " ==="] <> concatMap lines renderedDeclarations

renderDefinitionModulePath :: DefinitionSlice -> IO String
renderDefinitionModulePath definitionSlice =
  case definitionSliceRealSrcSpan definitionSlice of
    Nothing ->
      pure "<definition source unavailable>"
    Just realSrcSpan ->
      pure (relativeSourcePath (GHC.Plugins.unpackFS (GHC.srcSpanFile realSrcSpan)))

renderDefinitionBlock :: DeclarationSpans -> IO String
renderDefinitionBlock declarationSpanGroup = do
  declarationText <- definitionTextFromSpans declarationSpanGroup
  pure . unlines $
    ["--- " <> renderDeclarationBlockHeader declarationSpanGroup <> " ---"] <> lines declarationText

renderDeclarationBlockHeader :: DeclarationSpans -> String
renderDeclarationBlockHeader declarationSpanGroup =
  case declarationSpansLineRange declarationSpanGroup of
    Nothing ->
      "definition"
    Just (startLine, endLine) ->
      "lines " <> show startLine <> "-" <> show endLine

declarationSpansLineRange :: DeclarationSpans -> Maybe (Int, Int)
declarationSpansLineRange declarationSpanGroup = do
  firstSpan <- minimumMaybe realSrcSpans
  lastSpan <- maximumMaybe realSrcSpans
  pure (GHC.srcSpanStartLine firstSpan, GHC.srcSpanEndLine lastSpan)
  where
    realSrcSpans =
      mapMaybe realSrcSpanFromSrcSpan (maybeToList declarationSpanGroup.signatureSpan <> [declarationSpanGroup.declarationSpan])

definitionSliceRealSrcSpan :: DefinitionSlice -> Maybe GHC.RealSrcSpan
definitionSliceRealSrcSpan definitionSlice =
  case mapMaybe declarationSpansRealSrcSpan definitionSlice.declarationSpans of
    realSrcSpan : _ -> Just realSrcSpan
    [] -> Nothing

declarationSpansRealSrcSpan :: DeclarationSpans -> Maybe GHC.RealSrcSpan
declarationSpansRealSrcSpan declarationSpanGroup =
  realSrcSpanFromSrcSpan declarationSpanGroup.declarationSpan
    <|> (declarationSpanGroup.signatureSpan >>= realSrcSpanFromSrcSpan)

realSrcSpanFromSrcSpan :: GHC.SrcSpan -> Maybe GHC.RealSrcSpan
realSrcSpanFromSrcSpan = \case
  GHC.RealSrcSpan realSrcSpan _ -> Just realSrcSpan
  GHC.UnhelpfulSpan {} -> Nothing

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  sortOn (realSrcSpanFromSrcSpan . declarationSpan)

mergeRenderedDefinitionModules :: [DefinitionSlice] -> [DefinitionSlice]
mergeRenderedDefinitionModules =
  foldr insertSlice []
  where
    insertSlice slice [] = [slice]
    insertSlice slice (existing : rest)
      | existing.definitionModule == slice.definitionModule =
          case mergeDefinitionSlices [existing, slice] of
            Just merged -> merged : rest
            Nothing -> existing : rest
      | otherwise =
          existing : insertSlice slice rest

relativeSourcePath :: FilePath -> FilePath
relativeSourcePath sourcePath =
  case dropWhile (/= "src") (splitDirectories sourcePath) of
    [] -> sourcePath
    pathParts -> joinPath pathParts

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe values = Just (minimum values)

maximumMaybe :: (Ord a) => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe values = Just (maximum values)

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = nub

findFixtureSymbol :: String -> [Symbol] -> Maybe GHC.Name
findFixtureSymbol symbol =
  findFixtureSymbolInModule "Demo" symbol

findFixtureSymbolInModule :: String -> String -> [Symbol] -> Maybe GHC.Name
findFixtureSymbolInModule moduleName symbol =
  fmap name
    . find
      ( \matchedSymbol ->
          GHC.Plugins.getOccString matchedSymbol.name == symbol
            && maybe False ((== moduleName) . GHC.moduleNameString . GHC.moduleName) (GHC.Plugins.nameModule_maybe matchedSymbol.name)
      )

enableFlexibleInstances :: FilePath -> IO ()
enableFlexibleInstances fixtureRoot = do
  let packageFile = fixtureRoot </> "package.yaml"
  packageSource <- TIO.readFile packageFile
  TIO.writeFile
    packageFile
    (T.replace "- KindSignatures\n" "- KindSignatures\n- FlexibleInstances\n" packageSource)

referenceInstanceDefinitions :: T.Text
referenceInstanceDefinitions =
  T.unlines
    [ "",
      "instance HasIndex Support.SupportRecord where",
      "  toIndex _ =",
      "    Map.singleton (show Support.supportSeed) (Support.mkSupportRecord Support.supportSeed)"
    ]

mergedRootChainFixtureModuleSource :: T.Text
mergedRootChainFixtureModuleSource =
  T.unlines
    [ "module TestChain.Roots",
      "  ( Wrapped(..),",
      "    mkWrapped,",
      "    unwrapWrapped",
      "  )",
      "where",
      "",
      "newtype Wrapped = Wrapped Int",
      "",
      "mkWrapped :: Int -> Wrapped",
      "mkWrapped = Wrapped",
      "",
      "unwrapWrapped :: Wrapped -> Int",
      "unwrapWrapped (Wrapped value) = value"
    ]

usedInstanceClosureFixtureModuleSource :: T.Text
usedInstanceClosureFixtureModuleSource =
  T.unlines
    [ "module TestClosure.Render",
      "  ( Render(..),",
      "    renderInt",
      "  ) where",
      "",
      "class Render a where",
      "  render :: a -> String",
      "",
      "instance Render Int where",
      "  render value = \"int:\" <> show value",
      "",
      "instance Render Bool where",
      "  render value = if value then \"true\" else \"false\"",
      "",
      "renderInt :: Int -> String",
      "renderInt = render"
    ]

preciseReferenceMatchesFixtureModuleSource :: T.Text
preciseReferenceMatchesFixtureModuleSource =
  T.unlines
    [ "module TestRefs.Snippet",
      "  ( target,",
      "    useTarget",
      "  ) where",
      "",
      "target :: Int",
      "target = 1",
      "",
      "useTarget :: Int",
      "useTarget =",
      "  target",
      "    + target",
      "    + target"
    ]

matchedSpanStartLine :: GHC.SrcSpan -> Maybe Int
matchedSpanStartLine = \case
  GHC.RealSrcSpan realSpan _ -> Just (GHC.srcSpanStartLine realSpan)
  GHC.UnhelpfulSpan {} -> Nothing

matchedSpanLineRange :: GHC.SrcSpan -> Maybe (Int, Int)
matchedSpanLineRange = \case
  GHC.RealSrcSpan realSpan _ -> Just (GHC.srcSpanStartLine realSpan, GHC.srcSpanEndLine realSpan)
  GHC.UnhelpfulSpan {} -> Nothing

multilineRecordReferenceFixtureModuleSource :: T.Text
multilineRecordReferenceFixtureModuleSource =
  T.unlines
    [ "module TestRefs.RecordSnippet",
      "  ( Result(..),",
      "    build",
      "  ) where",
      "",
      "data Result = Result { fieldA :: Int, fieldB :: Int, fieldC :: Int, fieldD :: Int, fieldE :: Int, fieldF :: Int }",
      "",
      "build :: IO Result",
      "build = do",
      "  let res = Result",
      "        { fieldA = 1",
      "        , fieldB = 2",
      "        , fieldC = 3",
      "        , fieldD = 4",
      "        , fieldE = 5",
      "        , fieldF = 6",
      "        }",
      "  pure res"
    ]

multilineFunctionReferenceFixtureModuleSource :: T.Text
multilineFunctionReferenceFixtureModuleSource =
  T.unlines
    [ "module TestRefs.CallSnippet",
      "  ( target,",
      "    build",
      "  ) where",
      "",
      "target :: Int -> Int -> Int -> Int",
      "target a b c = a + b + c",
      "",
      "build :: Int",
      "build =",
      "  target",
      "    1",
      "    2",
      "    3"
    ]

nestedMultilineReferenceFixtureModuleSource :: T.Text
nestedMultilineReferenceFixtureModuleSource =
  T.unlines
    [ "module TestRefs.NestedSnippet",
      "  ( target,",
      "    build",
      "  ) where",
      "",
      "target :: Int",
      "target = 1",
      "",
      "build :: Int",
      "build =",
      "  consume",
      "    (Wrapper",
      "      { wrapped = target",
      "      , other = 2",
      "      })",
      "",
      "consume :: Wrapper -> Int",
      "consume wrapper = wrapped wrapper",
      "",
      "data Wrapper = Wrapper",
      "  { wrapped :: Int",
      "  , other :: Int",
      "  }"
    ]

caseSectionReferenceFixtureModuleSource :: T.Text
caseSectionReferenceFixtureModuleSource =
  T.unlines
    [ "module TestRefs.CaseSectionSnippet",
      "  ( target,",
      "    build",
      "  ) where",
      "",
      "target :: Int -> Int",
      "target value = value + 1",
      "",
      "build :: Maybe Int -> Int",
      "build maybeValue =",
      "  case maybeValue of",
      "    Nothing -> 0",
      "    Just value ->",
      "      target value",
      "        + 1"
    ]
