module DefinitionSpec (spec) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.List (find, intercalate, isInfixOf, nub, sort, sortOn)
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import qualified GHC.Plugins
import Lore.Definition (DeclarationSpans (..), DefinitionSlice (..), DefinitionSource (..), NamedDefinitionSource (..), ReferenceHit (..), ReferenceMatch (..), declarationSpans, definitionSourceModule, mergeDefinitionSlices, resolveDefinitionClosureSourcesNamed, resolveDefinitionSourceNamed, resolveReferenceMatchesForNames)
import Lore.Definition.RenderSlice (definitionSourceToRenderSlice)
import Lore.HomeModules (defaultLoadHomeModulesOptions)
import qualified Lore.HomeModules as HomeModules
import Lore.Internal.Definition.ProjectIndex (DefinitionTarget (..), dependenciesForNamedTarget, loadProjectDefinitionIndex, lookupDefinitionSource, lookupDefinitionTarget)
import Lore.Internal.Definition.Reachability (walkReachable)
import Lore.List (maximumMaybe, minimumMaybe)
import Lore.Lookup (Symbol (..))
import Lore.Monad (MonadLore)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, splitDirectories, (</>))
import Test.Hspec
import TestSupport (FixtureContext, findSymbols, fixtureLore, fixtureLoreAt, lookupRootSymbolChains, withFixtureCopy, withFixtureSpec)

loadHomeModules :: (MonadLore m) => HomeModules.LoadHomeModulesOptions -> m ()
loadHomeModules options = void (HomeModules.loadHomeModules options)

spec :: Spec
spec = withFixtureSpec do
  describe "walkReachable" do
    it "deduplicates roots while preserving first root ordering" \_fixture -> do
      walkReachable (Just 0) (const []) (["a", "b", "a", "c", "b"] :: [String])
        `shouldBe` [(0, "a"), (0, "b"), (0, "c")]

    it "preserves breadth-first queue ordering for broad dependencies" \_fixture -> do
      let neighbours :: String -> [String]
          neighbours "root" = map show [1 :: Int .. 5]
          neighbours _ = []

      walkReachable (Just 1) neighbours (["root"] :: [String])
        `shouldBe` [(0, "root"), (1, "1"), (1, "2"), (1, "3"), (1, "4"), (1, "5")]

    it "records transitive dependency depth" \_fixture -> do
      let neighbours :: String -> [String]
          neighbours "root" = ["direct"]
          neighbours "direct" = ["transitive"]
          neighbours _ = []

      walkReachable (Just 2) neighbours (["root"] :: [String])
        `shouldBe` [(0, "root"), (1, "direct"), (2, "transitive")]

    it "terminates cycles and keeps the first minimum-depth visit" \_fixture -> do
      let neighbours :: String -> [String]
          neighbours "root" = ["left", "right"]
          neighbours "left" = ["shared", "root"]
          neighbours "right" = ["shared"]
          neighbours "shared" = ["left"]
          neighbours _ = []

      walkReachable (Just 10) neighbours (["root"] :: [String])
        `shouldBe` [(0, "root"), (1, "left"), (1, "right"), (2, "shared")]

    it "keeps depth zero when a dependency is also requested as a root" \_fixture -> do
      let neighbours :: String -> [String]
          neighbours "root" = ["shared"]
          neighbours _ = []

      walkReachable (Just 2) neighbours (["shared", "root"] :: [String])
        `shouldBe` [(0, "shared"), (0, "root")]

  describe "resolveDefinitionSourceNamed + renderDefinitionSourceSlice" do
    it "resolves declaration spans for a symbol" \fixture -> do
      slice <- fixtureDefinition fixture "lookupOrZero"

      shouldHaveSingleDefinitionText
        slice
        "lookupOrZero pairs key =\n  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
        (Just "lookupOrZero :: [(String, Int)] -> String -> Int")
    it "resolves definitions that reference another module" \fixture -> do
      slice <- fixtureDefinition fixture "crossModuleRecord"

      shouldHaveSingleDefinitionText
        slice
        "crossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)"
        (Just "crossModuleRecord :: Int -> Support.SupportRecord")

    it "resolves definitions for unexported symbols from another home module" \fixture -> do
      slice <-
        fixtureLore fixture do
          loadHomeModules defaultLoadHomeModulesOptions
          symbols <- findSymbols "Demo.Support.supportValues"
          targetName <-
            maybe
              (error "symbol not found: Demo.Support.supportValues")
              pure
              (findFixtureSymbolInModule "Demo.Support" "supportValues" symbols)
          source <-
            maybe
              (error "definition not found: Demo.Support.supportValues")
              pure
              =<< resolveDefinitionSourceNamed targetName
          renderDefinitionSourceSlice source

      definitionTexts <- traverse definitionTextFromSpans slice.declarationSpans
      GHC.moduleNameString (GHC.moduleName slice.definitionModule) `shouldBe` "Demo.Support"
      any (isInfixOf "supportValues :: Map.Map String Int") definitionTexts `shouldBe` True

    it "includes references used inside a where block" \fixture -> do
      slice <- fixtureDefinition fixture "lookupWithWhere"

      shouldHaveSingleDefinitionText
        slice
        "lookupWithWhere pairs key =\n  fromMaybe fallback (Map.lookup key table)\n  where\n    table = Map.fromList pairs\n    fallback = Map.size table"
        (Just "lookupWithWhere :: [(String, Int)] -> String -> Int")

    it "resolves all clauses of a multi-equation top-level function" \fixture -> do
      slice <- fixtureDefinition fixture "isTrue"

      shouldHaveSingleDefinitionText
        slice
        "isTrue \"True\" = True\nisTrue \"False\" = False\nisTrue _ = False"
        (Just "isTrue :: String -> Bool")

    it "resolves the correct declaration for a type alias" \fixture -> do
      slice <- fixtureDefinition fixture "NameSet"

      shouldHaveSingleDefinitionText
        slice
        "type NameSet = Set.Set String"
        Nothing

    it "resolves the correct declaration for a type family" \fixture -> do
      slice <- fixtureDefinition fixture "Elem"

      shouldHaveSingleDefinitionText
        slice
        "type family Elem (container :: Type) :: Type"
        Nothing

    it "resolves the correct declaration for a data family" \fixture -> do
      slice <- fixtureDefinition fixture "Bucket"

      shouldHaveSingleDefinitionText
        slice
        "data family Bucket (item :: Type) :: Type"
        Nothing

    it "resolves the correct declaration for a data type" \fixture -> do
      slice <- fixtureDefinition fixture "Indexed"

      shouldHaveSingleDefinitionText
        slice
        "data Indexed a = Indexed\n  { indexedNames :: NameSet,\n    indexedValues :: Map.Map String a\n  }"
        Nothing

    it "resolves the correct declaration for a class" \fixture -> do
      slice <- fixtureDefinition fixture "HasIndex"

      shouldHaveSingleDefinitionText
        slice
        "class HasIndex a where\n  toIndex :: a -> Map.Map String a"
        Nothing

    it "survives two consecutive reloads before resolving a definition slice" \fixture -> do
      slice <-
        fixtureLore fixture do
          loadHomeModules defaultLoadHomeModulesOptions
          loadHomeModules defaultLoadHomeModulesOptions
          exportedSymbols <- findSymbols "lookupOrZero"
          targetName <- maybe (error "symbol not found: lookupOrZero") pure (findFixtureSymbol "lookupOrZero" exportedSymbols)
          source <- maybe (error "definition not found: lookupOrZero") pure =<< resolveDefinitionSourceNamed targetName
          renderDefinitionSourceSlice source

      shouldHaveSingleDefinitionText
        slice
        "lookupOrZero pairs key =\n  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
        (Just "lookupOrZero :: [(String, Int)] -> String -> Int")

    it "resolves a shared top-level pattern binding for the first bound name" \fixture -> do
      slice <- fixtureDefinition fixture "pairLeft"

      shouldHaveSingleDefinitionText
        slice
        "(pairLeft, pairRight) =\n  ( fromMaybe 0 (Map.lookup \"left\" table),\n    Map.size table\n  )\n  where\n    table = Map.fromList [(\"left\", 1), (\"right\", 2)]"
        (Just "pairLeft, pairRight :: Int")

    it "resolves a shared top-level pattern binding for the second bound name" \fixture -> do
      slice <- fixtureDefinition fixture "pairRight"

      shouldHaveSingleDefinitionText
        slice
        "(pairLeft, pairRight) =\n  ( fromMaybe 0 (Map.lookup \"left\" table),\n    Map.size table\n  )\n  where\n    table = Map.fromList [(\"left\", 1), (\"right\", 2)]"
        (Just "pairLeft, pairRight :: Int")

  describe "mergeDefinitionSlices" do
    it "merges declarations from the same module" \fixture -> do
      zero <- fixtureDefinition fixture "lookupOrZero"
      one <- fixtureDefinition fixture "lookupOrOne"

      case mergeDefinitionSlices [zero, one] of
        Just merged ->
          length (declarationSpans merged) `shouldBe` 2
        Nothing ->
          expectationFailure "expected merged slice"

    it "deduplicates repeated declaration spans when merged slices overlap" \fixture -> do
      zero <- fixtureDefinition fixture "lookupOrZero"

      case mergeDefinitionSlices [zero, zero] of
        Just merged ->
          length (declarationSpans merged) `shouldBe` 1
        Nothing ->
          expectationFailure "expected merged slice"

  describe "resolveDefinitionClosureSourcesNamed + renderDefinitionClosureSlices" do
    it "respects the requested recursion depth for same-module function references" \fixture -> do
      depthZero <- fixtureDefinitionClosure fixture 0 "derivedValue"
      depthOne <- fixtureDefinitionClosure fixture 1 "derivedValue"
      depthTwo <- fixtureDefinitionClosure fixture 2 "derivedValue"

      depthZero
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          ["derivedValue :: Int\nderivedValue = bumpWithSeed 2"]
                                        )
                                      ]
      depthOne
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "derivedValue :: Int\nderivedValue = bumpWithSeed 2",
                                            "bumpWithSeed :: Int -> Int\nbumpWithSeed value = value + seedValue"
                                          ]
                                        )
                                      ]
      depthTwo
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "derivedValue :: Int\nderivedValue = bumpWithSeed 2",
                                            "bumpWithSeed :: Int -> Int\nbumpWithSeed value = value + seedValue",
                                            "seedValue :: Int\nseedValue = 40"
                                          ]
                                        )
                                      ]

    it "records root, direct, and transitive dependency depths" \fixture -> do
      depths <-
        fixtureLore fixture do
          loadHomeModules defaultLoadHomeModulesOptions
          exportedSymbols <- findSymbols "derivedValue"
          targetName <- maybe (error "symbol not found: derivedValue") pure (findFixtureSymbol "derivedValue" exportedSymbols)
          namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
          pure [(GHC.Plugins.getOccString source.definitionName, source.definitionDependencyDepth) | source <- namedSources]

      depths `shouldSatisfy` elem ("derivedValue", 0)
      depths `shouldSatisfy` elem ("bumpWithSeed", 1)
      depths `shouldSatisfy` elem ("seedValue", 2)

    it "includes referenced types when recursively resolving a function definition" \fixture -> do
      closure <- fixtureDefinitionClosure fixture 1 "mkIndexed"

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "mkIndexed :: NameSet -> Indexed Int\nmkIndexed names =\n  Indexed\n    { indexedNames = names,\n      indexedValues = Map.empty\n    }",
                                            "type NameSet = Set.Set String",
                                            "data Indexed a = Indexed\n  { indexedNames :: NameSet,\n    indexedValues :: Map.Map String a\n  }"
                                          ]
                                        )
                                      ]

    it "resolves cross-module dependency targets from the merged project catalog" \fixture -> do
      (dependencyNames, allSourcesResolved) <-
        fixtureLore fixture do
          loadHomeModules defaultLoadHomeModulesOptions
          exportedSymbols <- findSymbols "crossModuleRecord"
          targetName <- maybe (error "symbol not found: crossModuleRecord") pure (findFixtureSymbol "crossModuleRecord" exportedSymbols)
          projectIndex <- loadProjectDefinitionIndex
          target <- maybe (error "project target not found: crossModuleRecord") pure (lookupDefinitionTarget projectIndex targetName)
          let dependencies = Set.toList (dependenciesForNamedTarget projectIndex target)
          pure
            ( map dependencyOccName dependencies,
              all (\dependency -> lookupDefinitionSource projectIndex (definitionTargetId dependency) /= Nothing) dependencies
            )

      dependencyNames `shouldContain` ["SupportRecord"]
      dependencyNames `shouldContain` ["mkSupportRecord"]
      dependencyNames `shouldContain` ["supportStep"]
      allSourcesResolved `shouldBe` True

    it "recurses through dependencies of the directly referenced constructor, not all constructors on the root declaration" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "ConstructorDeps.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile constructorScopedDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.ConstructorDeps.someFunction"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.ConstructorDeps.someFunction")
                pure
                (findFixtureSymbolInModule "TestClosure.ConstructorDeps" "someFunction" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.ConstructorDeps",
                                            [ "someFunction :: IO ()\nsomeFunction = do\n  let someBind = EitherBar undefined\n  print \"bar\"",
                                              "data EitherFooOrBar\n  = EitherFoo\n      Foo\n  | EitherBar\n      Bar",
                                              "data Bar = Bar"
                                            ]
                                          )
                                        ]

    it "recurses through dependencies of the referenced class method, not sibling methods on the same class" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "ClassDeps.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile classMethodScopedDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.ClassDeps.runAlpha"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.ClassDeps.runAlpha")
                pure
                (findFixtureSymbolInModule "TestClosure.ClassDeps" "runAlpha" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.ClassDeps",
                                            [ "runAlpha value = buildAlpha value",
                                              "class BuildResult a where\n  buildAlpha ::\n    a ->\n    AlphaResult\n  buildBeta ::\n    a ->\n    BetaResult",
                                              "data AlphaResult = AlphaResult"
                                            ]
                                          )
                                        ]

    it "recurses through dependencies of referenced constructors across module boundaries, not sibling constructors" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            supportFile = moduleDir </> "ConstructorSupport.hs"
            userFile = moduleDir </> "ConstructorUser.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile supportFile constructorSupportFixtureModuleSource
        TIO.writeFile userFile constructorUserFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.ConstructorUser.someFunction"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.ConstructorUser.someFunction")
                pure
                (findFixtureSymbolInModule "TestClosure.ConstructorUser" "someFunction" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 3 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.ConstructorSupport",
                                            [ "data EitherFooOrBar\n  = EitherFoo\n      Foo\n  | EitherBar\n      Bar",
                                              "data Bar = Bar"
                                            ]
                                          ),
                                          ( "TestClosure.ConstructorUser",
                                            [ "someFunction :: IO ()\nsomeFunction = do\n  let someBind = Support.EitherBar undefined\n  print \"bar\""
                                            ]
                                          )
                                        ]

    it "recurses from the second binder of a shared top-level declaration through root-scoped dependencies" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "SharedTopLevel.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile sharedTopLevelDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.SharedTopLevel.pairRight"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.SharedTopLevel.pairRight")
                pure
                (findFixtureSymbolInModule "TestClosure.SharedTopLevel" "pairRight" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.SharedTopLevel",
                                            [ "seedValue :: Int\nseedValue = 40",
                                              "mkLeft :: Int -> Int\nmkLeft value = value + seedValue",
                                              "mkRight :: Int -> Int\nmkRight value = value * seedValue",
                                              "pairLeft, pairRight :: Int\n(pairLeft, pairRight) =\n  (mkLeft seedValue, mkRight seedValue)"
                                            ]
                                          )
                                        ]

    it "recurses through dependencies of constructors declared in one shared GADT signature" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "SharedGadtConstructors.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile sharedGadtConstructorDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.SharedGadtConstructors.useB"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.SharedGadtConstructors.useB")
                pure
                (findFixtureSymbolInModule "TestClosure.SharedGadtConstructors" "useB" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.SharedGadtConstructors",
                                            [ "useB :: T\nuseB = B Foo",
                                              "data T where\n  A, B :: Foo -> T",
                                              "data Foo = Foo"
                                            ]
                                          )
                                        ]

    it "recurses from a directly requested GADT constructor through only that constructor's dependencies" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "DirectGadtConstructor.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile directGadtConstructorDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.DirectGadtConstructor.SomeMinimalTypedModuleFacts"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.DirectGadtConstructor.SomeMinimalTypedModuleFacts")
                pure
                (findFixtureSymbolInModule "TestClosure.DirectGadtConstructor" "SomeMinimalTypedModuleFacts" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 1 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.DirectGadtConstructor",
                                            [ "data MinimalTypedModuleFacts = MinimalTypedModuleFacts",
                                              "data SomeGADT a where\n  SomeParsedModuleFacts :: ParsedModuleFacts -> SomeGADT ParsedModuleFacts\n  SomeMinimalTypedModuleFacts :: MinimalTypedModuleFacts -> SomeGADT MinimalTypedModuleFacts"
                                            ]
                                          )
                                        ]

    it "recurses from a directly requested regular constructor through only that constructor's dependencies" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "DirectRegularConstructor.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile directRegularConstructorDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.DirectRegularConstructor.EitherBar"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.DirectRegularConstructor.EitherBar")
                pure
                (findFixtureSymbolInModule "TestClosure.DirectRegularConstructor" "EitherBar" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 1 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.DirectRegularConstructor",
                                            [ "data Bar = Bar",
                                              "data EitherFooOrBar\n  = EitherFoo\n      Foo\n  | EitherBar\n      Bar"
                                            ]
                                          )
                                        ]

    it "recurses from a directly requested class method through only that method's dependencies" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "DirectClassMethod.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile directClassMethodDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.DirectClassMethod.buildAlpha"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.DirectClassMethod.buildAlpha")
                pure
                (findFixtureSymbolInModule "TestClosure.DirectClassMethod" "buildAlpha" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 1 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.DirectClassMethod",
                                            [ "data AlphaResult = AlphaResult",
                                              "class BuildResult a where\n  buildAlpha ::\n    a ->\n    AlphaResult\n  buildBeta ::\n    a ->\n    BetaResult"
                                            ]
                                          )
                                        ]

    it "recurses from a directly requested record field through only that field's type dependencies" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "DirectRecordField.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile directRecordFieldDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.DirectRecordField.alphaField"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.DirectRecordField.alphaField")
                pure
                (findFixtureSymbolInModule "TestClosure.DirectRecordField" "alphaField" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.DirectRecordField",
                                            [ "data Alpha = Alpha",
                                              "data Record = Record\n  { alphaField :: !Alpha,\n    betaField :: !Beta\n  }"
                                            ]
                                          )
                                        ]

    it "recurses through dependencies of class methods declared in one shared signature" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "SharedClassSignature.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile sharedClassMethodDependencyFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.SharedClassSignature.runG"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.SharedClassSignature.runG")
                pure
                (findFixtureSymbolInModule "TestClosure.SharedClassSignature" "runG" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.SharedClassSignature",
                                            [ "runG value = g value",
                                              "class BuildResult a where\n  f, g :: a -> Result",
                                              "data Result = Result"
                                            ]
                                          )
                                        ]

    it "stops on already visited definitions when recursion encounters a cycle" \fixture -> do
      closure <- fixtureDefinitionClosure fixture 4 "mutualLeft"

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "mutualLeft :: Int -> Bool\nmutualLeft 0 = True\nmutualLeft n = mutualRight (n - 1)",
                                            "mutualRight :: Int -> Bool\nmutualRight 0 = False\nmutualRight n = mutualLeft (n - 1)"
                                          ]
                                        )
                                      ]

    it "recursively resolves referenced symbols across module boundaries" \fixture -> do
      closure <- fixtureDefinitionClosure fixture 2 "crossModuleRecord"

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "crossModuleRecord :: Int -> Support.SupportRecord\ncrossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)"
                                          ]
                                        ),
                                        ( "Demo.Support",
                                          [ "data SupportRecord = SupportRecord\n  { supportValues :: Map.Map String Int\n  }",
                                            "mkSupportRecord :: Int -> SupportRecord\nmkSupportRecord value =\n  SupportRecord\n    { supportValues = Map.singleton \"value\" value\n    }",
                                            "supportStep :: Int -> Int\nsupportStep value = value + supportSeed",
                                            "supportSeed :: Int\nsupportSeed = 5"
                                          ]
                                        )
                                      ]

    it "orders recursive closure results with the queried root definition first" \fixture -> do
      moduleOrder <-
        fixtureLore fixture do
          loadHomeModules defaultLoadHomeModulesOptions
          exportedSymbols <- findSymbols "crossModuleRecord"
          targetName <- maybe (error "symbol not found: crossModuleRecord") pure (findFixtureSymbol "crossModuleRecord" exportedSymbols)
          namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
          pure (map (GHC.moduleNameString . GHC.moduleName . definitionSourceModule . (.definitionSource)) namedSources)

      take 1 moduleOrder `shouldBe` ["Demo"]
      "Demo.Support" `shouldSatisfy` (`elem` moduleOrder)

    it "orders nested dependencies after their dependents within breadth-first closure output" \fixture -> do
      definitionOrder <-
        fixtureLore fixture do
          loadHomeModules defaultLoadHomeModulesOptions
          exportedSymbols <- findSymbols "derivedValue"
          targetName <- maybe (error "symbol not found: derivedValue") pure (findFixtureSymbol "derivedValue" exportedSymbols)
          namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
          pure (map (GHC.Plugins.getOccString . definitionName) namedSources)

      definitionOrder `shouldBe` ["derivedValue", "bumpWithSeed", "seedValue"]

    it "keeps direct dependencies before transitive dependencies in branching closures" \fixture -> do
      definitionOrder <-
        fixtureLore fixture do
          loadHomeModules defaultLoadHomeModulesOptions
          exportedSymbols <- findSymbols "crossModuleBundle"
          targetName <- maybe (error "symbol not found: crossModuleBundle") pure (findFixtureSymbol "crossModuleBundle" exportedSymbols)
          namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
          pure (map (GHC.Plugins.getOccString . definitionName) namedSources)

      let indexOf occName =
            maybe
              (error ("missing definition in closure: " <> occName))
              id
              (lookup occName (zip definitionOrder [0 :: Int ..]))
      indexOf "crossModuleSeed" `shouldSatisfy` (< indexOf "supportSeed")
      indexOf "crossModuleRecord" `shouldSatisfy` (< indexOf "supportStep")
      indexOf "crossModuleBundle" `shouldSatisfy` (< indexOf "crossModuleRecord")
      indexOf "crossModuleBundle" `shouldSatisfy` (< indexOf "crossModuleSeed")

    it "merges same-module closure declarations when dependencies are split across references" \fixture -> do
      closure <- fixtureDefinitionClosure fixture 2 "crossModuleBundle"

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "crossModuleBundle :: Int -> (Int, Support.SupportRecord)\ncrossModuleBundle value =\n  (crossModuleSeed, crossModuleRecord value)",
                                            "crossModuleRecord :: Int -> Support.SupportRecord\ncrossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)",
                                            "crossModuleSeed :: Int\ncrossModuleSeed = Support.supportSeed"
                                          ]
                                        ),
                                        ( "Demo.Support",
                                          [ "data SupportRecord = SupportRecord\n  { supportValues :: Map.Map String Int\n  }",
                                            "mkSupportRecord :: Int -> SupportRecord\nmkSupportRecord value =\n  SupportRecord\n    { supportValues = Map.singleton \"value\" value\n    }",
                                            "supportStep :: Int -> Int\nsupportStep value = value + supportSeed",
                                            "supportSeed :: Int\nsupportSeed = 5"
                                          ]
                                        )
                                      ]

    it "includes the concretely used class instance in recursive closure output" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "Render.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile usedInstanceClosureFixtureModuleSource

        closure <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.Render.renderInt"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.Render.renderInt")
                pure
                (findFixtureSymbolInModule "TestClosure.Render" "renderInt" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 1 targetName
            renderDefinitionClosureSlices namedSources

        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.Render",
                                            [ "class Render a where\n  render :: a -> String",
                                              "instance Render Int where\n  render value = \"int:\" <> show value",
                                              "renderInt :: Int -> String\nrenderInt = render"
                                            ]
                                          )
                                        ]

    it "survives two consecutive reloads before resolving a definition closure" \fixture -> do
      closure <-
        fixtureLore fixture do
          loadHomeModules defaultLoadHomeModulesOptions
          loadHomeModules defaultLoadHomeModulesOptions
          exportedSymbols <- findSymbols "crossModuleRecord"
          targetName <- maybe (error "symbol not found: crossModuleRecord") pure (findFixtureSymbol "crossModuleRecord" exportedSymbols)
          namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
          renderDefinitionClosureSlices namedSources

      closure
        `shouldHaveModuleDefinitions` [ ( "Demo",
                                          [ "crossModuleRecord :: Int -> Support.SupportRecord\ncrossModuleRecord value =\n  Support.mkSupportRecord (Support.supportStep value)"
                                          ]
                                        ),
                                        ( "Demo.Support",
                                          [ "data SupportRecord = SupportRecord\n  { supportValues :: Map.Map String Int\n  }",
                                            "mkSupportRecord :: Int -> SupportRecord\nmkSupportRecord value =\n  SupportRecord\n    { supportValues = Map.singleton \"value\" value\n    }",
                                            "supportStep :: Int -> Int\nsupportStep value = value + supportSeed",
                                            "supportSeed :: Int\nsupportSeed = 5"
                                          ]
                                        )
                                      ]

    it "traverses two reached names on the same declaration independently" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "SharedReachedBinders.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile sharedReachedBindersFixtureModuleSource

        (occNames, closure) <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestClosure.SharedReachedBinders.root"
            targetName <-
              maybe
                (error "symbol not found: TestClosure.SharedReachedBinders.root")
                pure
                (findFixtureSymbolInModule "TestClosure.SharedReachedBinders" "root" exportedSymbols)
            namedSources <- resolveDefinitionClosureSourcesNamed 2 targetName
            rendered <- renderDefinitionClosureSlices namedSources
            pure (map (GHC.Plugins.getOccString . definitionName) namedSources, rendered)

        occNames `shouldContain` ["memberA"]
        occNames `shouldContain` ["memberB"]
        closure
          `shouldHaveModuleDefinitions` [ ( "TestClosure.SharedReachedBinders",
                                            [ "data TypeA = TypeA",
                                              "data TypeB = TypeB",
                                              "data Box\n  = BoxA TypeA\n  | BoxB TypeB",
                                              "memberA, memberB :: Box\n(memberA, memberB) = (BoxA TypeA, BoxB TypeB)",
                                              "root :: (Box, Box)\nroot = (memberA, memberB)"
                                            ]
                                          )
                                        ]

    it "applies depth boundaries while keeping shorter-path expansion available" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestClosure"
            moduleFile = moduleDir </> "DepthBoundaries.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile depthBoundaryFixtureModuleSource

        depthZero <- fixtureClosureOccNamesInModule fixture fixtureRoot "TestClosure.DepthBoundaries" "root" 0
        depthOne <- fixtureClosureOccNamesInModule fixture fixtureRoot "TestClosure.DepthBoundaries" "root" 1
        depthTwo <- fixtureClosureOccNamesInModule fixture fixtureRoot "TestClosure.DepthBoundaries" "root" 2
        depthThree <- fixtureClosureOccNamesInModule fixture fixtureRoot "TestClosure.DepthBoundaries" "root" 3

        depthZero `shouldBe` ["root"]
        depthOne `shouldMatchList` ["direct", "root"]
        depthTwo `shouldMatchList` ["direct", "leaf", "root"]
        depthThree `shouldMatchList` ["direct", "leaf", "root"]

  describe "resolveReferenceDefinitions" do
    it "finds top-level definitions and instance definitions that reference the target" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableFlexibleInstances fixtureRoot
        TIO.appendFile demoFile referenceInstanceDefinitions

        references <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "Demo.Support.supportSeed"
            targetName <-
              maybe
                (error "symbol not found: Demo.Support.supportSeed")
                pure
                (findFixtureSymbolInModule "Demo.Support" "supportSeed" exportedSymbols)
            resolveReferenceMatchesForNames [targetName]

        map referenceMatchDefinition references
          `shouldHaveModuleDefinitionSources` [ ( "Demo",
                                                  ["crossModuleSeed :: Int\ncrossModuleSeed = Support.supportSeed"]
                                                ),
                                                ( "Demo",
                                                  ["instance HasIndex Support.SupportRecord where\n  toIndex _ =\n    Map.singleton (show Support.supportSeed) (Support.mkSupportRecord Support.supportSeed)"]
                                                ),
                                                ( "Demo.Support",
                                                  ["supportStep :: Int -> Int\nsupportStep value = value + supportSeed"]
                                                )
                                              ]

    it "merges root chains with the same root before matching references" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestChain"
            moduleFile = moduleDir </> "Roots.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile mergedRootChainFixtureModuleSource

        references <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            resolvedRootChains <- lookupRootSymbolChains "TestChain.Roots.Wrapped"
            case resolvedRootChains of
              [rootChain] ->
                resolveReferenceMatchesForNames rootChain
              _ ->
                error ("unexpected resolved roots count: " <> show (length resolvedRootChains))

        map referenceMatchDefinition references
          `shouldHaveModuleDefinitionSources` [ ( "TestChain.Roots",
                                                  ["mkWrapped :: Int -> Wrapped\nmkWrapped = Wrapped"]
                                                ),
                                                ( "TestChain.Roots",
                                                  ["unwrapWrapped :: Wrapped -> Int\nunwrapWrapped (Wrapped value) = value"]
                                                )
                                              ]
        let occurrenceKeys =
              [ (referenceHitTargetName occurrence, show occurrence.referenceHitExactSpan)
              | reference <- references,
                occurrence <- reference.referenceMatchOccurrences
              ]
        length occurrenceKeys `shouldBe` length (nub occurrenceKeys)

  describe "resolveReferenceMatches" do
    it "returns exact matched reference spans for each occurrence in a definition" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestRefs"
            moduleFile = moduleDir </> "Snippet.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile preciseReferenceMatchesFixtureModuleSource

        referenceMatches <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestRefs.Snippet.target"
            targetName <-
              maybe
                (error "symbol not found: TestRefs.Snippet.target")
                pure
                (findFixtureSymbolInModule "TestRefs.Snippet" "target" exportedSymbols)
            resolveReferenceMatchesForNames [targetName]

        case referenceMatches of
          [referenceMatch] -> do
            GHC.moduleNameString (GHC.moduleName (definitionSourceModule referenceMatch.referenceMatchDefinition)) `shouldBe` "TestRefs.Snippet"
            sort (mapMaybe matchedSpanStartLine (referenceMatchExactSpans referenceMatch)) `shouldBe` [11, 12, 13]
          other ->
            expectationFailure ("expected a single definition-level reference match, got: " <> show (length other))

    it "returns exact reference spans inside case alternatives" \fixture -> do
      withFixtureCopy fixture \fixtureRoot -> do
        let moduleDir = fixtureRoot </> "src" </> "TestRefs"
            moduleFile = moduleDir </> "CaseSectionSnippet.hs"
        createDirectoryIfMissing True moduleDir
        TIO.writeFile moduleFile caseSectionReferenceFixtureModuleSource

        referenceMatches <-
          fixtureLoreAt fixture fixtureRoot do
            loadHomeModules defaultLoadHomeModulesOptions
            exportedSymbols <- findSymbols "TestRefs.CaseSectionSnippet.target"
            targetName <-
              maybe
                (error "symbol not found: TestRefs.CaseSectionSnippet.target")
                pure
                (findFixtureSymbolInModule "TestRefs.CaseSectionSnippet" "target" exportedSymbols)
            resolveReferenceMatchesForNames [targetName]

        sort (concatMap (mapMaybe matchedSpanStartLine . referenceMatchExactSpans) referenceMatches) `shouldBe` [14]

  describe "renderDefinitionModulesText" do
    it "renders a single definition as a minified module fragment" \fixture -> do
      rendered <- fixtureRenderedDefinition fixture "lookupOrZero"

      rendered
        `shouldBe` unlines
          [ "=== src/Demo.hs ===",
            "--- lines 33-35 ---",
            "lookupOrZero :: [(String, Int)] -> String -> Int",
            "lookupOrZero pairs key =",
            "  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
          ]

    it "renders recursive closures grouped by file" \fixture -> do
      rendered <- fixtureRenderedDefinitionClosure fixture 2 "crossModuleRecord"

      rendered
        `shouldBe` unlines
          [ "=== src/Demo.hs ===",
            "--- lines 60-62 ---",
            "crossModuleRecord :: Int -> Support.SupportRecord",
            "crossModuleRecord value =",
            "  Support.mkSupportRecord (Support.supportStep value)",
            "",
            "=== src/Demo/Support.hs ===",
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
            "    }"
          ]

shouldHaveSingleDefinitionText ::
  DefinitionSlice ->
  String ->
  Maybe String ->
  IO ()
shouldHaveSingleDefinitionText slice expectedDeclaration expectedSignature =
  case slice.declarationSpans of
    [spans] -> do
      declarationText <- readSpanText spans.declarationSpan
      signatureText <- traverse readSpanText spans.signatureSpan
      declarationText `shouldBe` expectedDeclaration
      signatureText `shouldBe` expectedSignature
    spans ->
      expectationFailure ("Expected one declaration span, got " <> show (length spans))

shouldHaveModuleDefinitions :: [DefinitionSlice] -> [(String, [String])] -> IO ()
shouldHaveModuleDefinitions slices expectedDefinitions = do
  actualDefinitions <- traverse renderedModuleDefinition slices
  fmap normalizeModuleDefinition actualDefinitions
    `shouldMatchList` fmap normalizeModuleDefinition expectedDefinitions

renderedModuleDefinition :: DefinitionSlice -> IO (String, [String])
renderedModuleDefinition slice = do
  texts <- traverse definitionTextFromSpans slice.declarationSpans
  pure
    ( GHC.moduleNameString (GHC.moduleName slice.definitionModule),
      texts
    )

normalizeModuleDefinition :: (String, [String]) -> (String, [String])
normalizeModuleDefinition (moduleName, definitions) =
  (moduleName, sort definitions)

shouldHaveModuleDefinitionSources :: [DefinitionSource] -> [(String, [String])] -> IO ()
shouldHaveModuleDefinitionSources sources expectedDefinitions = do
  actualDefinitions <- traverse renderedModuleDefinitionSource sources
  fmap normalizeModuleDefinitionSource actualDefinitions
    `shouldMatchList` fmap normalizeModuleDefinitionSource expectedDefinitions

renderedModuleDefinitionSource :: DefinitionSource -> IO (String, [String])
renderedModuleDefinitionSource source = do
  text <- definitionTextFromSpans source.definitionSourceSpans
  pure
    ( GHC.moduleNameString (GHC.moduleName (definitionSourceModule source)),
      [text]
    )

normalizeModuleDefinitionSource :: (String, [String]) -> (String, [String])
normalizeModuleDefinitionSource (moduleName, definitions) =
  (moduleName, sort definitions)

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

fixtureDefinition :: FixtureContext -> String -> IO DefinitionSlice
fixtureDefinition fixture symbol =
  fixtureLore fixture do
    loadHomeModules defaultLoadHomeModulesOptions
    exportedSymbols <- findSymbols (pack symbol)
    targetName <- maybe (error ("symbol not found: " <> symbol)) pure (findFixtureSymbol symbol exportedSymbols)
    source <- maybe (error ("definition not found: " <> symbol)) pure =<< resolveDefinitionSourceNamed targetName
    renderDefinitionSourceSlice source

fixtureDefinitionClosure :: FixtureContext -> Int -> String -> IO [DefinitionSlice]
fixtureDefinitionClosure fixture depth symbol =
  fixtureLore fixture do
    loadHomeModules defaultLoadHomeModulesOptions
    exportedSymbols <- findSymbols (pack symbol)
    targetName <- maybe (error ("symbol not found: " <> symbol)) pure (findFixtureSymbol symbol exportedSymbols)
    namedSources <- resolveDefinitionClosureSourcesNamed depth targetName
    renderDefinitionClosureSlices namedSources

fixtureClosureOccNamesInModule :: FixtureContext -> FilePath -> String -> String -> Int -> IO [String]
fixtureClosureOccNamesInModule fixture fixtureRoot moduleName symbol depth =
  fixtureLoreAt fixture fixtureRoot do
    loadHomeModules defaultLoadHomeModulesOptions
    exportedSymbols <- findSymbols (pack (moduleName <> "." <> symbol))
    targetName <-
      maybe
        (error ("symbol not found: " <> moduleName <> "." <> symbol))
        pure
        (findFixtureSymbolInModule moduleName symbol exportedSymbols)
    namedSources <- resolveDefinitionClosureSourcesNamed depth targetName
    pure (map (GHC.Plugins.getOccString . definitionName) namedSources)

dependencyOccName :: DefinitionTarget -> String
dependencyOccName =
  GHC.Plugins.getOccString . definitionTargetName

renderDefinitionSourceSlice :: (MonadLore m) => DefinitionSource -> m DefinitionSlice
renderDefinitionSourceSlice source =
  pure (definitionSourceToRenderSlice source)

renderDefinitionClosureSlices :: (MonadLore m) => [NamedDefinitionSource] -> m [DefinitionSlice]
renderDefinitionClosureSlices namedSources = do
  renderedSlices <- mapM (renderDefinitionSourceSlice . (.definitionSource)) namedSources
  pure (mergeRenderedDefinitionModules renderedSlices)

fixtureRenderedDefinition :: FixtureContext -> String -> IO String
fixtureRenderedDefinition fixture symbol =
  renderDefinitionSlicesText . pure =<< fixtureDefinition fixture symbol

fixtureRenderedDefinitionClosure :: FixtureContext -> Int -> String -> IO String
fixtureRenderedDefinitionClosure fixture depth symbol =
  renderDefinitionSlicesText =<< fixtureDefinitionClosure fixture depth symbol

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

constructorScopedDependencyFixtureModuleSource :: T.Text
constructorScopedDependencyFixtureModuleSource =
  T.unlines
    [ "module TestClosure.ConstructorDeps",
      "  ( someFunction",
      "  ) where",
      "",
      "data Foo = Foo",
      "",
      "data Bar = Bar",
      "",
      "data EitherFooOrBar",
      "  = EitherFoo",
      "      Foo",
      "  | EitherBar",
      "      Bar",
      "",
      "someFunction :: IO ()",
      "someFunction = do",
      "  let someBind = EitherBar undefined",
      "  print \"bar\""
    ]

classMethodScopedDependencyFixtureModuleSource :: T.Text
classMethodScopedDependencyFixtureModuleSource =
  T.unlines
    [ "module TestClosure.ClassDeps",
      "  ( runAlpha",
      "  ) where",
      "",
      "data AlphaResult = AlphaResult",
      "",
      "data BetaResult = BetaResult",
      "",
      "class BuildResult a where",
      "  buildAlpha ::",
      "    a ->",
      "    AlphaResult",
      "  buildBeta ::",
      "    a ->",
      "    BetaResult",
      "",
      "runAlpha value = buildAlpha value"
    ]

constructorSupportFixtureModuleSource :: T.Text
constructorSupportFixtureModuleSource =
  T.unlines
    [ "module TestClosure.ConstructorSupport",
      "  ( Foo (..),",
      "    Bar (..),",
      "    EitherFooOrBar (..)",
      "  ) where",
      "",
      "data Foo = Foo",
      "",
      "data Bar = Bar",
      "",
      "data EitherFooOrBar",
      "  = EitherFoo",
      "      Foo",
      "  | EitherBar",
      "      Bar"
    ]

constructorUserFixtureModuleSource :: T.Text
constructorUserFixtureModuleSource =
  T.unlines
    [ "module TestClosure.ConstructorUser",
      "  ( someFunction",
      "  ) where",
      "",
      "import qualified TestClosure.ConstructorSupport as Support",
      "",
      "someFunction :: IO ()",
      "someFunction = do",
      "  let someBind = Support.EitherBar undefined",
      "  print \"bar\""
    ]

sharedTopLevelDependencyFixtureModuleSource :: T.Text
sharedTopLevelDependencyFixtureModuleSource =
  T.unlines
    [ "module TestClosure.SharedTopLevel",
      "  ( pairRight",
      "  ) where",
      "",
      "seedValue :: Int",
      "seedValue = 40",
      "",
      "mkLeft :: Int -> Int",
      "mkLeft value = value + seedValue",
      "",
      "mkRight :: Int -> Int",
      "mkRight value = value * seedValue",
      "",
      "pairLeft, pairRight :: Int",
      "(pairLeft, pairRight) =",
      "  (mkLeft seedValue, mkRight seedValue)"
    ]

sharedGadtConstructorDependencyFixtureModuleSource :: T.Text
sharedGadtConstructorDependencyFixtureModuleSource =
  T.unlines
    [ "{-# LANGUAGE GADTs #-}",
      "",
      "module TestClosure.SharedGadtConstructors",
      "  ( useB",
      "  ) where",
      "",
      "data Foo = Foo",
      "",
      "data T where",
      "  A, B :: Foo -> T",
      "",
      "useB :: T",
      "useB = B Foo"
    ]

sharedReachedBindersFixtureModuleSource :: T.Text
sharedReachedBindersFixtureModuleSource =
  T.unlines
    [ "module TestClosure.SharedReachedBinders",
      "  ( root",
      "  ) where",
      "",
      "data TypeA = TypeA",
      "",
      "data TypeB = TypeB",
      "",
      "data Box",
      "  = BoxA TypeA",
      "  | BoxB TypeB",
      "",
      "memberA, memberB :: Box",
      "(memberA, memberB) = (BoxA TypeA, BoxB TypeB)",
      "",
      "root :: (Box, Box)",
      "root = (memberA, memberB)"
    ]

depthBoundaryFixtureModuleSource :: T.Text
depthBoundaryFixtureModuleSource =
  T.unlines
    [ "module TestClosure.DepthBoundaries",
      "  ( root",
      "  ) where",
      "",
      "root :: Int",
      "root = direct",
      "",
      "direct :: Int",
      "direct = leaf",
      "",
      "leaf :: Int",
      "leaf = direct"
    ]

directGadtConstructorDependencyFixtureModuleSource :: T.Text
directGadtConstructorDependencyFixtureModuleSource =
  T.unlines
    [ "{-# LANGUAGE GADTs #-}",
      "",
      "module TestClosure.DirectGadtConstructor",
      "  ( SomeGADT (SomeMinimalTypedModuleFacts),",
      "  ) where",
      "",
      "data ParsedModuleFacts = ParsedModuleFacts",
      "",
      "data MinimalTypedModuleFacts = MinimalTypedModuleFacts",
      "",
      "data SomeGADT a where",
      "  SomeParsedModuleFacts :: ParsedModuleFacts -> SomeGADT ParsedModuleFacts",
      "  SomeMinimalTypedModuleFacts :: MinimalTypedModuleFacts -> SomeGADT MinimalTypedModuleFacts"
    ]

directRegularConstructorDependencyFixtureModuleSource :: T.Text
directRegularConstructorDependencyFixtureModuleSource =
  T.unlines
    [ "module TestClosure.DirectRegularConstructor",
      "  ( EitherFooOrBar (EitherBar),",
      "  ) where",
      "",
      "data Foo = Foo",
      "",
      "data Bar = Bar",
      "",
      "data EitherFooOrBar",
      "  = EitherFoo",
      "      Foo",
      "  | EitherBar",
      "      Bar"
    ]

directClassMethodDependencyFixtureModuleSource :: T.Text
directClassMethodDependencyFixtureModuleSource =
  T.unlines
    [ "module TestClosure.DirectClassMethod",
      "  ( BuildResult (buildAlpha),",
      "  ) where",
      "",
      "data AlphaResult = AlphaResult",
      "",
      "data BetaResult = BetaResult",
      "",
      "class BuildResult a where",
      "  buildAlpha ::",
      "    a ->",
      "    AlphaResult",
      "  buildBeta ::",
      "    a ->",
      "    BetaResult"
    ]

directRecordFieldDependencyFixtureModuleSource :: T.Text
directRecordFieldDependencyFixtureModuleSource =
  T.unlines
    [ "module TestClosure.DirectRecordField",
      "  ( alphaField,",
      "  ) where",
      "",
      "data Alpha = Alpha",
      "",
      "data Beta = Beta",
      "",
      "data Record = Record",
      "  { alphaField :: !Alpha,",
      "    betaField :: !Beta",
      "  }"
    ]

sharedClassMethodDependencyFixtureModuleSource :: T.Text
sharedClassMethodDependencyFixtureModuleSource =
  T.unlines
    [ "module TestClosure.SharedClassSignature",
      "  ( runG",
      "  ) where",
      "",
      "data Result = Result",
      "",
      "class BuildResult a where",
      "  f, g :: a -> Result",
      "",
      "runG value = g value"
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

referenceMatchExactSpans :: ReferenceMatch -> [GHC.SrcSpan]
referenceMatchExactSpans =
  map (.referenceHitExactSpan) . (.referenceMatchOccurrences)

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
