module GetDefinitionSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Mcp.Tools.GetDefinition.Cached (cachedGetDefinitionTool)
import Lore.Mcp.Tools.GetDefinition.Regular (regularGetDefinitionTool)
import Lore.Mcp.Tools.LookupSymbolInfo (lookupSymbolInfoTool)
import Lore.Mcp.Tools.NotifyKnowledgeReset (notifyKnowledgeResetTool)
import McpTestSupport
  ( callToolWithArgs,
    callToolWithoutArgs,
    fixtureLoreMcp,
    fixtureLoreMcpAtWithCache,
    fixtureLoreMcpWithCache,
    loadFixtureTargets,
    withFixtureCopy,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec
import Text.Printf (printf)

spec :: Spec
spec = do
  describe "getDefinition (cached mode)" do
    it "omits already returned definitions and force=true returns them again" do
      (firstCall, secondCall, forcedCall) <-
        fixtureLoreMcpWithCache True do
          loadFixtureTargets
          firstCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 Nothing)
          secondCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 Nothing)
          forcedCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 (Just True))
          pure (firstCall, secondCall, forcedCall)

      firstCall `shouldContainText` "lookupOrZero"
      secondCall `shouldContainText` "already returned earlier in this MCP session"
      secondCall `shouldContainText` "Demo: lookupOrZero"
      forcedCall `shouldContainText` "lookupOrZero"

    it "remembers recursively returned definitions per symbol and omits them in later direct requests" do
      (recursiveCall, directCall) <-
        fixtureLoreMcpWithCache True do
          loadFixtureTargets
          recursiveCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["derivedValue"] 2 Nothing)
          directCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["bumpWithSeed"] 0 Nothing)
          pure (recursiveCall, directCall)

      recursiveCall `shouldContainText` "bumpWithSeed :: Int -> Int"
      directCall `shouldContainText` "already returned earlier in this MCP session"
      directCall `shouldContainText` "Demo: bumpWithSeed"

    it "returns previously omitted definitions after notifyKnowledgeReset" do
      (cachedCall, resetCall, afterResetCall) <-
        fixtureLoreMcpWithCache True do
          loadFixtureTargets
          _ <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrOne"] 0 Nothing)
          cachedCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrOne"] 0 Nothing)
          resetCall <- callToolWithoutArgs notifyKnowledgeResetTool
          afterResetCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgs ["lookupOrOne"] 0 Nothing)
          pure (cachedCall, resetCall, afterResetCall)

      cachedCall `shouldContainText` "already returned earlier in this MCP session"
      resetCall `shouldContainText` "Knowledge reset acknowledged. Cleared "
      afterResetCall `shouldContainText` "lookupOrOne"

    it "keeps non-rendered paginated definitions uncached until they are actually shown" do
      withFixtureCopy \fixtureRoot -> do
        addPaginatedDefinitionFixture fixtureRoot
        (firstPageCall, secondPageCall) <-
          fixtureLoreMcpAtWithCache True fixtureRoot do
            loadFixtureTargets
            firstPageCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgsWithSkip paginatedDefinitionSymbols (Just 0) 0 Nothing)
            secondPageCall <- callToolWithArgs cachedGetDefinitionTool (getDefinitionArgsWithSkip paginatedDefinitionSymbols (Just 30) 0 Nothing)
            pure (firstPageCall, secondPageCall)

        firstPageCall `shouldContainText` "pageDef30 :: Int"
        firstPageCall `shouldNotContainText` "pageDef31 :: Int"
        secondPageCall `shouldContainText` "Showing all 1 definition results."
        secondPageCall `shouldContainText` "pageDef31 :: Int"

  describe "getDefinition (regular mode)" do
    it "does not suppress repeated definitions across calls" do
      (firstCall, secondCall) <-
        fixtureLoreMcp do
          loadFixtureTargets
          firstCall <- callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 Nothing)
          secondCall <- callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["lookupOrZero"] 0 Nothing)
          pure (firstCall, secondCall)

      firstCall `shouldContainText` "lookupOrZero"
      secondCall `shouldContainText` "lookupOrZero"
      secondCall `shouldNotContainText` "already returned earlier in this MCP session"

    it "follows constructor-specific recursive dependencies while still rendering root declarations" do
      withFixtureCopy \fixtureRoot -> do
        addConstructorScopedDependencyFixture fixtureRoot
        definitionResult <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["TestClosure.ConstructorDeps.someFunction"] 2 Nothing)

        definitionResult `shouldContainText` "someFunction :: IO ()"
        definitionResult `shouldContainText` "data EitherFooOrBar"
        definitionResult `shouldContainText` "data Bar = Bar"
        definitionResult `shouldNotContainText` "data Foo = Foo"

    it "follows class-method-specific recursive dependencies while still rendering root declarations" do
      withFixtureCopy \fixtureRoot -> do
        addClassMethodScopedDependencyFixture fixtureRoot
        definitionResult <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["TestClosure.ClassDeps.runAlpha"] 2 Nothing)

        definitionResult `shouldContainText` "runAlpha value = buildAlpha value"
        definitionResult `shouldContainText` "class BuildResult a where"
        definitionResult `shouldContainText` "data AlphaResult = AlphaResult"
        definitionResult `shouldNotContainText` "data BetaResult = BetaResult"

    it "follows cross-module constructor-specific recursive dependencies while still rendering root declarations" do
      withFixtureCopy \fixtureRoot -> do
        addCrossModuleConstructorScopedDependencyFixture fixtureRoot
        definitionResult <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["TestClosure.ConstructorUser.someFunction"] 3 Nothing)

        definitionResult `shouldContainText` "someFunction :: IO ()"
        definitionResult `shouldContainText` "data EitherFooOrBar"
        definitionResult `shouldContainText` "data Bar = Bar"
        definitionResult `shouldNotContainText` "data Foo = Foo"

    it "follows dependencies from the second binder of a shared top-level declaration" do
      withFixtureCopy \fixtureRoot -> do
        addSharedTopLevelDependencyFixture fixtureRoot
        definitionResult <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["TestClosure.SharedTopLevel.pairRight"] 2 Nothing)

        definitionResult `shouldContainText` "pairLeft, pairRight :: Int"
        definitionResult `shouldContainText` "mkLeft :: Int -> Int"
        definitionResult `shouldContainText` "mkRight :: Int -> Int"
        definitionResult `shouldContainText` "seedValue :: Int"

    it "follows record-field-specific recursive dependencies without root-resolving the field to its record" do
      withFixtureCopy \fixtureRoot -> do
        addRecordFieldDependencyFixture fixtureRoot
        definitionResult <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["TestClosure.RecordFieldDeps.alphaField"] 1 Nothing)

        definitionResult `shouldContainText` "data Record = Record"
        definitionResult `shouldContainText` "alphaField :: !Alpha"
        definitionResult `shouldContainText` "betaField :: !Beta"
        definitionResult `shouldContainText` "data Alpha = Alpha"
        definitionResult `shouldNotContainText` "data Beta = Beta"

    it "returns all same-module matches for a duplicated qualified symbol name (regression for resolveRequestedSymbols)" do
      withFixtureCopy \fixtureRoot -> do
        addSameModuleDuplicateSymbolFixture fixtureRoot
        (lookupResult, definitionResult) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            lookupResult <- callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "Demo.AmbiguousId")
            definitionResult <- callToolWithArgs regularGetDefinitionTool (getDefinitionArgs ["Demo.AmbiguousId"] 0 Nothing)
            pure (lookupResult, definitionResult)

        lookupResult `shouldContainText` "Found 3 symbol candidates:"
        definitionResult `shouldContainText` "type AmbiguousId = Int"
        definitionResult `shouldContainText` "data instance AmbiguousField AmbiguousRec AmbiguousId = AmbiguousId"
        definitionResult `shouldNotContainText` "is ambiguous. More qualification is required"

  describe "lookupSymbolInfo" do
    it "resolves exported and unexported record fields from DuplicateRecordFields modules" do
      withFixtureCopy \fixtureRoot -> do
        addRecordFieldLookupFixture fixtureRoot
        (exportedFieldLookup, unexportedFieldLookup, qualifiedFieldLookup, constructorLookup) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            exportedFieldLookup <- callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "userName")
            unexportedFieldLookup <- callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "hiddenValue")
            qualifiedFieldLookup <- callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "Demo.hiddenValue")
            constructorLookup <- callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "Demo.Hidden")
            pure (exportedFieldLookup, unexportedFieldLookup, qualifiedFieldLookup, constructorLookup)

        exportedFieldLookup `shouldContainText` "userName"
        exportedFieldLookup `shouldContainText` "Found 1 symbol candidates:"
        exportedFieldLookup `shouldContainText` ":: User -> String"

        unexportedFieldLookup `shouldContainText` "hiddenValue"
        unexportedFieldLookup `shouldContainText` ":: Hidden -> Int"
        unexportedFieldLookup `shouldNotContainText` "type Hidden :: Type"

        qualifiedFieldLookup `shouldContainText` "hiddenValue"
        qualifiedFieldLookup `shouldContainText` "Found 1 symbol candidates:"
        qualifiedFieldLookup `shouldContainText` ":: Hidden -> Int"
        qualifiedFieldLookup `shouldNotContainText` "type Hidden :: Type"

        constructorLookup `shouldContainText` "Hidden"
        constructorLookup `shouldContainText` "type Hidden :: Type"

    it "keeps lookup for existing unexported top-level home-module symbols" do
      supportValuesLookup <-
        fixtureLoreMcp do
          loadFixtureTargets
          callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "supportValues")

      supportValuesLookup `shouldContainText` "supportValues"
      supportValuesLookup `shouldContainText` "Found 1 symbol candidates:"
      supportValuesLookup `shouldNotContainText` "type SupportRecord :: Type"

    it "suggests similar symbols when exact lookup misses" do
      supportValuesLookup <-
        fixtureLoreMcp do
          loadFixtureTargets
          callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "supportVlaues")

      supportValuesLookup `shouldContainText` "No symbols found for \"supportVlaues\"."
      supportValuesLookup `shouldContainText` "Maybe you meant one of these?"
      supportValuesLookup `shouldContainText` "supportValues"

    it "suggests each lookup occurrence once and hides generated symbols" do
      withFixtureCopy \fixtureRoot -> do
        addSuggestionDuplicateFixture fixtureRoot
        suggestedLookup <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "suggestedsSymbol")

        suggestedLookup `shouldContainText` "No symbols found for \"suggestedsSymbol\"."
        suggestedLookup `shouldContainText` "SuggestedSymbol"
        T.count "SuggestedSymbol" suggestedLookup `shouldBe` 1
        suggestedLookup `shouldNotContainText` "$tc"
        generatedLookup <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "repSuggestedSymbol")
        generatedLookup `shouldContainText` "SuggestedSymbol"
        generatedLookup `shouldNotContainText` "Rep_SuggestedSymbol"

    it "returns both exact symbols and selector aliases for ambiguous occurrence queries" do
      withFixtureCopy \fixtureRoot -> do
        addRecordFieldLookupFixture fixtureRoot
        addSupportHiddenValueFixture fixtureRoot
        (unqualifiedLookup, qualifiedLookup) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureTargets
            unqualifiedLookup <- callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "hiddenValue")
            qualifiedLookup <- callToolWithArgs lookupSymbolInfoTool (lookupSymbolInfoArgs "Demo.hiddenValue")
            pure (unqualifiedLookup, qualifiedLookup)

        unqualifiedLookup `shouldContainText` "Found 2 symbol candidates:"
        unqualifiedLookup `shouldContainText` ":: Hidden -> Int"
        unqualifiedLookup `shouldContainText` "hiddenValue :: Int"

        qualifiedLookup `shouldContainText` "Found 1 symbol candidates:"
        qualifiedLookup `shouldContainText` ":: Hidden -> Int"
        qualifiedLookup `shouldNotContainText` "hiddenValue :: Int"

lookupSymbolInfoArgs :: Text -> J.Value
lookupSymbolInfoArgs symbol =
  J.object
    [ "symbol" J..= symbol
    ]

addSameModuleDuplicateSymbolFixture :: FilePath -> IO ()
addSameModuleDuplicateSymbolFixture fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  source <- TIO.readFile demoFile
  TIO.writeFile demoFile (source <> "\n\n" <> sameModuleDuplicateFixtureDeclarations)

addSuggestionDuplicateFixture :: FilePath -> IO ()
addSuggestionDuplicateFixture fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  source <- TIO.readFile demoFile
  let sourceWithDeriveGeneric =
        "{-# LANGUAGE DeriveGeneric #-}\n"
          <> T.replace "import Data.Kind (Type)" "import Data.Kind (Type)\nimport GHC.Generics (Generic)" source
  TIO.writeFile demoFile (sourceWithDeriveGeneric <> "\n\n" <> suggestionDuplicateFixtureDeclarations)

suggestionDuplicateFixtureDeclarations :: Text
suggestionDuplicateFixtureDeclarations =
  T.unlines
    [ "data SuggestedSymbol = SuggestedSymbol",
      "  deriving (Generic)"
    ]

sameModuleDuplicateFixtureDeclarations :: Text
sameModuleDuplicateFixtureDeclarations =
  T.unlines
    [ "data AmbiguousRec = AmbiguousRec",
      "",
      "type AmbiguousId = Int",
      "",
      "data family AmbiguousField rec typ",
      "",
      "data instance AmbiguousField AmbiguousRec AmbiguousId = AmbiguousId"
    ]

getDefinitionArgs :: [Text] -> Int -> Maybe Bool -> J.Value
getDefinitionArgs symbols recursionDepth maybeForce =
  getDefinitionArgsWithSkip symbols Nothing recursionDepth maybeForce

getDefinitionArgsWithSkip :: [Text] -> Maybe Int -> Int -> Maybe Bool -> J.Value
getDefinitionArgsWithSkip symbols maybeSkip recursionDepth maybeForce =
  J.object $
    [ "symbols" J..= symbols,
      "recursionDepth" J..= recursionDepth
    ]
      <> case maybeSkip of
        Nothing -> []
        Just skipValue -> ["skip" J..= skipValue]
      <> case maybeForce of
        Nothing -> []
        Just forceValue -> ["force" J..= forceValue]

addPaginatedDefinitionFixture :: FilePath -> IO ()
addPaginatedDefinitionFixture fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  source <- TIO.readFile demoFile
  let sourceWithExports =
        T.replace paginationExportAnchor paginationExportReplacement source
  TIO.writeFile demoFile (sourceWithExports <> "\n\n" <> paginatedDefinitionsFixtureDeclarations)

addConstructorScopedDependencyFixture :: FilePath -> IO ()
addConstructorScopedDependencyFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestClosure"
      moduleFile = moduleDir </> "ConstructorDeps.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile constructorScopedDependencyFixtureModuleSource

addClassMethodScopedDependencyFixture :: FilePath -> IO ()
addClassMethodScopedDependencyFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestClosure"
      moduleFile = moduleDir </> "ClassDeps.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile classMethodScopedDependencyFixtureModuleSource

addCrossModuleConstructorScopedDependencyFixture :: FilePath -> IO ()
addCrossModuleConstructorScopedDependencyFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestClosure"
      supportFile = moduleDir </> "ConstructorSupport.hs"
      userFile = moduleDir </> "ConstructorUser.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile supportFile constructorSupportFixtureModuleSource
  TIO.writeFile userFile constructorUserFixtureModuleSource

addSharedTopLevelDependencyFixture :: FilePath -> IO ()
addSharedTopLevelDependencyFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestClosure"
      moduleFile = moduleDir </> "SharedTopLevel.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile sharedTopLevelDependencyFixtureModuleSource

addRecordFieldDependencyFixture :: FilePath -> IO ()
addRecordFieldDependencyFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestClosure"
      moduleFile = moduleDir </> "RecordFieldDeps.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile recordFieldDependencyFixtureModuleSource

addRecordFieldLookupFixture :: FilePath -> IO ()
addRecordFieldLookupFixture fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  source <- TIO.readFile demoFile
  let sourceWithDuplicateRecordFields =
        if T.isInfixOf "{-# LANGUAGE DuplicateRecordFields #-}" source
          then source
          else "{-# LANGUAGE DuplicateRecordFields #-}\n" <> source
      sourceWithExports =
        T.replace recordFieldLookupExportAnchor recordFieldLookupExportReplacement sourceWithDuplicateRecordFields
  TIO.writeFile demoFile (sourceWithExports <> "\n\n" <> recordFieldLookupFixtureDeclarations)

addSupportHiddenValueFixture :: FilePath -> IO ()
addSupportHiddenValueFixture fixtureRoot = do
  let supportFile = fixtureRoot </> "src" </> "Demo" </> "Support.hs"
  source <- TIO.readFile supportFile
  let sourceWithExports =
        T.replace supportHiddenValueExportAnchor supportHiddenValueExportReplacement source
  TIO.writeFile supportFile (sourceWithExports <> "\n\n" <> supportHiddenValueFixtureDeclarations)

constructorScopedDependencyFixtureModuleSource :: Text
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

classMethodScopedDependencyFixtureModuleSource :: Text
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

constructorSupportFixtureModuleSource :: Text
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

constructorUserFixtureModuleSource :: Text
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

sharedTopLevelDependencyFixtureModuleSource :: Text
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

recordFieldDependencyFixtureModuleSource :: Text
recordFieldDependencyFixtureModuleSource =
  T.unlines
    [ "module TestClosure.RecordFieldDeps",
      "  ( alphaField",
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

recordFieldLookupExportAnchor :: Text
recordFieldLookupExportAnchor =
  T.unlines
    [ "    HasIndex (..),",
      "  )",
      "where"
    ]

recordFieldLookupExportReplacement :: Text
recordFieldLookupExportReplacement =
  T.unlines
    [ "    HasIndex (..),",
      "    User(..),",
      "    Hidden(Hidden),",
      "    publicFn,",
      "  )",
      "where"
    ]

recordFieldLookupFixtureDeclarations :: Text
recordFieldLookupFixtureDeclarations =
  T.unlines
    [ "data User = User",
      "  { userName :: String",
      "  }",
      "",
      "data Hidden = Hidden",
      "  { hiddenValue :: Int",
      "  }",
      "",
      "data LeftTagged = LeftTagged",
      "  { sharedValue :: Int",
      "  }",
      "",
      "data RightTagged = RightTagged",
      "  { sharedValue :: String",
      "  }",
      "",
      "publicFn :: Hidden -> Int",
      "publicFn = hiddenValue"
    ]

supportHiddenValueExportAnchor :: Text
supportHiddenValueExportAnchor =
  T.unlines
    [ "    mkSupportRecord,",
      "    (.+.),",
      "  )"
    ]

supportHiddenValueExportReplacement :: Text
supportHiddenValueExportReplacement =
  T.unlines
    [ "    mkSupportRecord,",
      "    hiddenValue,",
      "    (.+.),",
      "  )"
    ]

supportHiddenValueFixtureDeclarations :: Text
supportHiddenValueFixtureDeclarations =
  T.unlines
    [ "hiddenValue :: Int",
      "hiddenValue = supportSeed * 10"
    ]

paginationExportAnchor :: Text
paginationExportAnchor =
  T.unlines
    [ "    HasIndex (..),",
      "  )",
      "where"
    ]

paginationExportReplacement :: Text
paginationExportReplacement =
  T.unlines $
    [ "    HasIndex (..),"
    ]
      <> map (\symbolName -> "    " <> symbolName <> ",") paginatedDefinitionSymbols
      <> [ "  )",
           "where"
         ]

paginatedDefinitionSymbols :: [Text]
paginatedDefinitionSymbols =
  [ T.pack (printf "pageDef%02d" index :: String)
  | index <- [1 :: Int .. 31]
  ]

paginatedDefinitionsFixtureDeclarations :: Text
paginatedDefinitionsFixtureDeclarations =
  T.unlines $
    concat
      [ [ symbolName <> " :: Int",
          symbolName <> " = " <> T.pack (show index),
          ""
        ]
      | (index, symbolName) <- zip [1 :: Int ..] paginatedDefinitionSymbols
      ]

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle =
  haystack `shouldSatisfy` T.isInfixOf needle

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText haystack needle =
  haystack `shouldSatisfy` (not . T.isInfixOf needle)
