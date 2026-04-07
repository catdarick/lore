module TargetsSpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Diagnostics (Diagnostic (..), DiagnosticClass (..), DiagnosticSpan (..), Span (..))
import Lore.Logger (LogMessage (..), LoggerHandle (..))
import Lore.Lookup (findSymbols)
import Lore.Targets (LoadTargetsOptions (..), defaultLoadTargetsOptions, loadTargets, retainUnresolvedRollback)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (fixtureLoreAt, fixtureLoreAtWithLogger, withFixtureCopy)

spec :: Spec
spec =
  describe "loadTargets auto-refact" do
    it "re-adds a missing unqualified import when the symbol has a unique module" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        rewriteDemo demoFile $
          T.unlines
            . filter (/= "import Data.Maybe (fromMaybe)")
            . T.lines

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "import Data.Maybe (fromMaybe)\n" demoSource `shouldBe` True

    it "opens a qualified aliased import when a used item is missing" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        rewriteDemo demoFile $
          T.replace
            "explicitQualified ch =\n  Set.member ch (Set.fromList \"abc\")"
            "explicitQualified ch =\n  Set.member ch (Set.fromList \"abc\") || Set.member ch (Set.empty :: Set.Set Char)"

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "explicitQualified"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        countImportHeaders "import qualified Data.Set as Set" demoSource `shouldBe` 1
        importHeaderFor "import qualified Data.Set as Set" demoSource
          `shouldBe` Just "import qualified Data.Set as Set"

    it "does not prefer an already imported module over a better qualified match" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableTextDependency fixtureRoot
        enableOverloadedStrings demoFile
        addImportAndKeepDefinition
          demoFile
          "import Data.List (find)\n"
          [ "qualifiedTextJoin :: T.Text",
            "qualifiedTextJoin = T.intercalate \", \" [T.pack \"a\", T.pack \"b\"]"
          ]

        logsRef <- newIORef []
        let loggerHandle =
              LoggerHandle \logMessage ->
                modifyIORef' logsRef (<> [logMessage.content])

        loaded <- fixtureLoreAtWithLogger loggerHandle fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        logs <- readIORef logsRef
        if loaded
          then pure ()
          else expectationFailure (unlines logs <> "\n" <> T.unpack demoSource)
        countImportHeaders "import qualified Data.Text as T" demoSource `shouldBe` 1
        countImportHeaders "import qualified Data.List as T" demoSource `shouldBe` 0

    it "merges multiple missing names from the same new module into one import" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureAutoRefactFixtureModule fixtureRoot
        appendDemoDefinitions
          demoFile
          [ "missingFixtureCombo :: FixtureType -> Int",
            "missingFixtureCombo _ = fixtureValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        countImportHeaders "import AutoRefactFixture.Imports" demoSource `shouldBe` 1
        importHeaderFor "import AutoRefactFixture.Imports" demoSource
          `shouldSatisfy` maybe False (\line -> "FixtureType" `T.isInfixOf` line && "fixtureValue" `T.isInfixOf` line)

    it "extends an existing unqualified import for a uniquely matching reexported symbol" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureReexportFixtureModules fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.ReexportLongName (ReexportTag)\n"
          [ "reexportedViaExistingImport :: Int",
            "reexportedViaExistingImport = reexportedValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        countImportHeaders "import AutoRefactFixture.ReexportLongName" demoSource `shouldBe` 1
        importHeaderFor "import AutoRefactFixture.ReexportLongName" demoSource
          `shouldSatisfy` maybe False (\line -> "ReexportTag" `T.isInfixOf` line && "reexportedValue" `T.isInfixOf` line)
        countImportHeaders "import AutoRefactFixture.ReA" demoSource `shouldBe` 0
        countImportHeaders "import AutoRefactFixture.ReexportedBase" demoSource `shouldBe` 0

    it "uses the shortest module name for a uniquely matching reexported symbol when no import exists" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureReexportFixtureModules fixtureRoot
        appendDemoDefinitions
          demoFile
          [ "reexportedViaShortestModule :: Int",
            "reexportedViaShortestModule = reexportedValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "import AutoRefactFixture.ReA (reexportedValue)\n" demoSource `shouldBe` True
        countImportHeaders "import AutoRefactFixture.ReexportLongName" demoSource `shouldBe` 0
        countImportHeaders "import AutoRefactFixture.ReexportedBase" demoSource `shouldBe` 0

    it "adds an import for a missing type constructor" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureAutoRefactFixtureModule fixtureRoot
        appendDemoDefinitions
          demoFile
          [ "missingFixtureType :: FixtureType -> Int",
            "missingFixtureType _ = 1"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "import AutoRefactFixture.Imports (FixtureType)\n" demoSource `shouldBe` True

    it "adds an import for a missing class" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureAutoRefactFixtureModule fixtureRoot
        appendDemoDefinitions
          demoFile
          [ "missingFixtureClass :: FixtureClass a => a -> Int",
            "missingFixtureClass _ = 1"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "import AutoRefactFixture.Imports (FixtureClass)\n" demoSource `shouldBe` True

    it "adds an import via the parent type for a missing data constructor" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureAutoRefactFixtureModule fixtureRoot
        appendDemoDefinitions
          demoFile
          [ "missingFixtureCtor :: FixtureBox",
            "missingFixtureCtor = FixtureCtorA"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        importHeaderFor "import AutoRefactFixture.Imports" demoSource
          `shouldSatisfy` maybe False (\line -> "FixtureBox" `T.isInfixOf` line && "FixtureCtorA" `T.isInfixOf` line)

    it "does not edit ambiguous missing imports" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureAmbiguousFixtureModules fixtureRoot
        appendDemoDefinitions
          demoFile
          [ "ambiguousFixtureRef :: Int",
            "ambiguousFixtureRef = ambiguousFixtureValue"
          ]
        originalSource <- TIO.readFile demoFile

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` False
        demoSource `shouldBe` originalSource

    it "does not use an existing import to disambiguate a bare missing symbol" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableTextDependency fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import Data.Text (Text)\n"
          [ "ambiguousFind :: Maybe Char",
            "ambiguousFind = find (== 'a') \"abc\""
          ]
        originalSource <- TIO.readFile demoFile

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` False
        demoSource `shouldBe` originalSource

    it "rolls back unresolved auto-refact edits when the build still fails" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        rewriteDemo demoFile $
          \source ->
            T.unlines
              (filter (/= "import Data.Maybe (fromMaybe)") (T.lines source))
              <> "\n"
              <> T.unlines
                [ "stillBroken :: Int",
                  "stillBroken = doesNotExistAnywhere"
                ]
        originalSource <- TIO.readFile demoFile

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` False
        demoSource `shouldBe` originalSource

    it "preserves rollback state only for files that still fail" do
      let rollbackState :: Map.Map FilePath T.Text
          rollbackState =
            Map.fromList
              [ ("src/Demo.hs", T.pack "demo-original"),
                ("src/Demo/Support.hs", T.pack "support-original")
              ]
          diagnostics =
            [ diagnosticIn "src/Demo/Support.hs",
              diagnosticIn "src/Demo/Support.hs"
            ]

      retainUnresolvedRollback rollbackState diagnostics
        `shouldBe` Map.fromList [("src/Demo/Support.hs", T.pack "support-original")]

    it "removes a whole redundant import" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        rewriteDemo demoFile $
          T.replace
            "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
            ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                <> "import qualified Data.IntMap.Strict as IntMap\n"
            )

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "import qualified Data.IntMap.Strict as IntMap" demoSource `shouldBe` False

    it "removes a redundant qualified import" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        rewriteDemo demoFile $
          T.replace
            "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
            ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                <> "import qualified Data.Sequence as Seq\n"
            )

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "import qualified Data.Sequence as Seq" demoSource `shouldBe` False

    it "removes a redundant type item from an explicit import list" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.Imports (FixtureType, fixtureValue)\n"
          [ "keepFixtureValueForTypeImport :: Int",
            "keepFixtureValueForTypeImport = fixtureValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "FixtureType" demoSource `shouldBe` False
        T.isInfixOf "import AutoRefactFixture.Imports (fixtureValue)\n" demoSource `shouldBe` True

    it "removes a redundant class item from an explicit import list" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.Imports (FixtureClass, fixtureValue)\n"
          [ "keepFixtureValueForClassImport :: Int",
            "keepFixtureValueForClassImport = fixtureValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "FixtureClass" demoSource `shouldBe` False
        T.isInfixOf "import AutoRefactFixture.Imports (fixtureValue)\n" demoSource `shouldBe` True

    it "removes redundant bindings from an explicit import list" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        rewriteDemo demoFile $
          T.replace
            "import Data.Maybe (fromMaybe)"
            "import Data.Maybe (fromMaybe, maybe)"

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "import Data.Maybe (fromMaybe, maybe)" demoSource `shouldBe` False
        T.isInfixOf "import Data.Maybe (fromMaybe)\n" demoSource `shouldBe` True

    it "removes multiple redundant bindings from a single import list" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        rewriteDemo demoFile $
          T.replace
            "import Data.Maybe (fromMaybe)"
            "import Data.Maybe (fromMaybe, maybe, listToMaybe)"

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "maybe" demoSource `shouldBe` False
        T.isInfixOf "listToMaybe" demoSource `shouldBe` False
        T.isInfixOf "import Data.Maybe (fromMaybe)\n" demoSource `shouldBe` True

    it "extends an existing explicit import instead of adding a second import line" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        appendDemoDefinitions
          demoFile
          [ "firstValue :: [(String, Int)] -> Maybe Int",
            "firstValue pairs = listToMaybe (map snd pairs)"
          ]

        logsRef <- newIORef []
        let loggerHandle =
              LoggerHandle \logMessage ->
                modifyIORef' logsRef (<> [logMessage.content])

        loaded <- fixtureLoreAtWithLogger loggerHandle fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        logs <- readIORef logsRef
        if loaded
          then pure ()
          else expectationFailure (unlines logs <> "\n" <> T.unpack demoSource)
        countImportHeaders "import Data.Maybe" demoSource `shouldBe` 1
        importHeaderFor "import Data.Maybe" demoSource
          `shouldSatisfy` maybe False (\line -> "fromMaybe" `T.isInfixOf` line && "listToMaybe" `T.isInfixOf` line)

    it "adds a separate unqualified import when the module is only imported qualified" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import qualified AutoRefactFixture.Imports as Fixture (FixtureType)\n"
          [ "qualifiedOnlyFixtureType :: Fixture.FixtureType -> Int",
            "qualifiedOnlyFixtureType _ = fixtureValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        countImportHeaders "import qualified AutoRefactFixture.Imports as Fixture" demoSource `shouldBe` 1
        importHeaderFor "import qualified AutoRefactFixture.Imports as Fixture" demoSource
          `shouldSatisfy` maybe False (\line -> "fixtureValue" `T.isInfixOf` line == False)
        countImportHeaders "import AutoRefactFixture.Imports" demoSource `shouldBe` 1
        importHeaderFor "import AutoRefactFixture.Imports" demoSource
          `shouldSatisfy` maybe False (\line -> "fixtureValue" `T.isInfixOf` line)

    it "adds a separate unqualified import when the module is already imported with an open qualified alias" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import qualified AutoRefactFixture.Imports as Fixture\n"
          [ "qualifiedFixtureValue :: Int",
            "qualifiedFixtureValue = Fixture.fixtureValue",
            "unqualifiedFixtureValue :: Int",
            "unqualifiedFixtureValue = fixtureValue"
          ]

        logsRef <- newIORef []
        let loggerHandle =
              LoggerHandle \logMessage ->
                modifyIORef' logsRef (<> [logMessage.content])

        loaded <- fixtureLoreAtWithLogger loggerHandle fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        logs <- readIORef logsRef
        if loaded
          then pure ()
          else expectationFailure (unlines logs <> "\n" <> T.unpack demoSource)
        countImportHeaders "import qualified AutoRefactFixture.Imports as Fixture" demoSource `shouldBe` 1
        countImportHeaders "import AutoRefactFixture.Imports" demoSource `shouldBe` 1
        importHeaderFor "import AutoRefactFixture.Imports" demoSource
          `shouldSatisfy` maybe False (\line -> "fixtureValue" `T.isInfixOf` line)

    it "adds a separate unqualified import when the module uses postfix qualified alias syntax" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.Imports qualified as Fixture\n"
          [ "qualifiedFixtureType :: Fixture.FixtureType -> Int",
            "qualifiedFixtureType _ = 1",
            "postfixQualifiedFixtureValue :: Int",
            "postfixQualifiedFixtureValue = fixtureValue"
          ]

        logsRef <- newIORef []
        let loggerHandle =
              LoggerHandle \logMessage ->
                modifyIORef' logsRef (<> [logMessage.content])

        loaded <- fixtureLoreAtWithLogger loggerHandle fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        logs <- readIORef logsRef
        if loaded
          then pure ()
          else expectationFailure (unlines logs <> "\n" <> T.unpack demoSource)
        countImportHeaders "import AutoRefactFixture.Imports qualified as Fixture" demoSource `shouldBe` 1
        countImportHeaders "import AutoRefactFixture.Imports (" demoSource `shouldBe` 1
        importHeaderFor "import AutoRefactFixture.Imports (" demoSource
          `shouldSatisfy` maybe False (\line -> "fixtureValue" `T.isInfixOf` line)

    it "removes a redundant operator import item" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.Imports ((.+.), fixtureValue)\n"
          [ "keepFixtureValueForOperatorImport :: Int",
            "keepFixtureValueForOperatorImport = fixtureValue"
          ]

        logsRef <- newIORef []
        let loggerHandle =
              LoggerHandle \logMessage ->
                modifyIORef' logsRef (<> [logMessage.content])

        loaded <- fixtureLoreAtWithLogger loggerHandle fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        logs <- readIORef logsRef
        if loaded
          then pure ()
          else expectationFailure (unlines logs <> "\n" <> T.unpack demoSource)
        T.isInfixOf ".+." demoSource `shouldBe` False
        T.isInfixOf "import AutoRefactFixture.Imports (fixtureValue)\n" demoSource `shouldBe` True

    it "removes a redundant pattern import item" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        enablePatternSynonyms demoFile
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.Imports (pattern FixturePat, fixtureValue)\n"
          [ "keepFixtureValueForPatternImport :: Int",
            "keepFixtureValueForPatternImport = fixtureValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "pattern FixturePat" demoSource `shouldBe` False
        T.isInfixOf "import AutoRefactFixture.Imports (fixtureValue)\n" demoSource `shouldBe` True

    it "removes a redundant parent-child import item" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.Imports (FixtureBox(FixtureCtorA, FixtureCtorB), fixtureValue)\n"
          [ "keepFixtureCtorA :: FixtureBox",
            "keepFixtureCtorA = FixtureCtorA",
            "keepFixtureValueForThingWithImport :: Int",
            "keepFixtureValueForThingWithImport = fixtureValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "FixtureCtorB" demoSource `shouldBe` False
        T.isInfixOf "FixtureCtorA" demoSource `shouldBe` True

    it "removes a redundant all-constructors import item" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        ensureAutoRefactFixtureModule fixtureRoot
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.Imports (FixtureAll(..), fixtureValue)\n"
          [ "keepFixtureValueForAllImport :: Int",
            "keepFixtureValueForAllImport = fixtureValue"
          ]

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "FixtureAll(..)" demoSource `shouldBe` False
        T.isInfixOf "import AutoRefactFixture.Imports (fixtureValue)\n" demoSource `shouldBe` True

    it "edits multi-line import lists safely" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        enableWarningErrors fixtureRoot
        rewriteDemo demoFile $
          T.replace
            "import Data.Maybe (fromMaybe)"
            ( T.unlines
                [ "import Data.Maybe",
                  "  ( maybe,",
                  "    fromMaybe,",
                  "    listToMaybe",
                  "  )"
                ]
            )

        loaded <- fixtureLoreAt fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "maybe," demoSource `shouldBe` False
        T.isInfixOf "listToMaybe" demoSource `shouldBe` False
        T.isInfixOf "fromMaybe" demoSource `shouldBe` True

rewriteDemo :: FilePath -> (T.Text -> T.Text) -> IO ()
rewriteDemo filePath f = do
  source <- TIO.readFile filePath
  TIO.writeFile filePath (f source)

appendDemoDefinitions :: FilePath -> [T.Text] -> IO ()
appendDemoDefinitions demoFile definitions =
  rewriteDemo demoFile (\source -> source <> "\n" <> T.unlines definitions)

addImportAndKeepDefinition :: FilePath -> T.Text -> [T.Text] -> IO ()
addImportAndKeepDefinition demoFile importText definitions =
  rewriteDemo demoFile $
    \source ->
      T.replace
        "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
        ("import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n" <> importText)
        source
        <> "\n"
        <> T.unlines definitions

enableWarningErrors :: FilePath -> IO ()
enableWarningErrors fixtureRoot = do
  let packageFile = fixtureRoot </> "package.yaml"
  packageSource <- TIO.readFile packageFile
  TIO.writeFile packageFile $
    T.replace
      "library:\n  source-dirs: src\n"
      "ghc-options:\n- -Werror\n- -Wunused-imports\n\nlibrary:\n  source-dirs: src\n"
      packageSource

enableTextDependency :: FilePath -> IO ()
enableTextDependency fixtureRoot = do
  let packageFile = fixtureRoot </> "package.yaml"
  packageSource <- TIO.readFile packageFile
  TIO.writeFile packageFile $
    T.replace
      "- containers\n"
      "- containers\n- text\n"
      packageSource

ensureAutoRefactFixtureModule :: FilePath -> IO ()
ensureAutoRefactFixtureModule fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "AutoRefactFixture"
      moduleFile = moduleDir </> "Imports.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile autoRefactFixtureModuleSource

enablePatternSynonyms :: FilePath -> IO ()
enablePatternSynonyms demoFile =
  rewriteDemo demoFile ("{-# LANGUAGE PatternSynonyms #-}\n" <>)

enableOverloadedStrings :: FilePath -> IO ()
enableOverloadedStrings demoFile =
  rewriteDemo demoFile ("{-# LANGUAGE OverloadedStrings #-}\n" <>)

ensureAmbiguousFixtureModules :: FilePath -> IO ()
ensureAmbiguousFixtureModules fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "AutoRefactFixture"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile (moduleDir </> "AmbiguousA.hs") (ambiguousFixtureModuleSource "AutoRefactFixture.AmbiguousA")
  TIO.writeFile (moduleDir </> "AmbiguousB.hs") (ambiguousFixtureModuleSource "AutoRefactFixture.AmbiguousB")

ensureReexportFixtureModules :: FilePath -> IO ()
ensureReexportFixtureModules fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "AutoRefactFixture"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile (moduleDir </> "ReexportedBase.hs") reexportedBaseModuleSource
  TIO.writeFile (moduleDir </> "ReA.hs") reexportedShortModuleSource
  TIO.writeFile (moduleDir </> "ReexportLongName.hs") reexportedLongModuleSource

autoRefactFixtureModuleSource :: T.Text
autoRefactFixtureModuleSource =
  T.unlines
    [ "{-# LANGUAGE PatternSynonyms #-}",
      "module AutoRefactFixture.Imports",
      "  ( fixtureValue,",
      "    FixtureType,",
      "    FixtureClass,",
      "    FixtureBox(FixtureCtorA, FixtureCtorB),",
      "    FixtureAll(..),",
      "    pattern FixturePat,",
      "    (.+.)",
      "  )",
      "where",
      "",
      "fixtureValue :: Int",
      "fixtureValue = 11",
      "",
      "data FixtureType = FixtureType",
      "",
      "class FixtureClass a where",
      "  fixtureClassMethod :: a -> Int",
      "",
      "instance FixtureClass Int where",
      "  fixtureClassMethod = id",
      "",
      "data FixtureBox = FixtureCtorA | FixtureCtorB",
      "",
      "data FixtureAll = FixtureAllA | FixtureAllB",
      "",
      "pattern FixturePat :: Int",
      "pattern FixturePat = 11",
      "",
      "(.+.) :: Int -> Int -> Int",
      "left .+. right = left + right",
      "",
      "infixl 6 .+."
    ]

ambiguousFixtureModuleSource :: T.Text -> T.Text
ambiguousFixtureModuleSource moduleName =
  T.unlines
    [ "module " <> moduleName,
      "  ( ambiguousFixtureValue",
      "  )",
      "where",
      "",
      "ambiguousFixtureValue :: Int",
      "ambiguousFixtureValue = 1"
    ]

reexportedBaseModuleSource :: T.Text
reexportedBaseModuleSource =
  T.unlines
    [ "module AutoRefactFixture.ReexportedBase",
      "  ( reexportedValue,",
      "    ReexportTag(..)",
      "  )",
      "where",
      "",
      "reexportedValue :: Int",
      "reexportedValue = 7",
      "",
      "data ReexportTag = ReexportTag"
    ]

reexportedShortModuleSource :: T.Text
reexportedShortModuleSource =
  T.unlines
    [ "module AutoRefactFixture.ReA",
      "  ( reexportedValue,",
      "    ReexportTag(..)",
      "  )",
      "where",
      "",
      "import AutoRefactFixture.ReexportedBase",
      "  ( reexportedValue,",
      "    ReexportTag(..)",
      "  )"
    ]

reexportedLongModuleSource :: T.Text
reexportedLongModuleSource =
  T.unlines
    [ "module AutoRefactFixture.ReexportLongName",
      "  ( reexportedValue,",
      "    ReexportTag(..)",
      "  )",
      "where",
      "",
      "import AutoRefactFixture.ReexportedBase",
      "  ( reexportedValue,",
      "    ReexportTag(..)",
      "  )"
    ]

countImportHeaders :: T.Text -> T.Text -> Int
countImportHeaders prefix =
  length . filter (prefix `T.isPrefixOf`) . T.lines

importHeaderFor :: T.Text -> T.Text -> Maybe T.Text
importHeaderFor prefix =
  findLine (prefix `T.isPrefixOf`) . T.lines

findLine :: (a -> Bool) -> [a] -> Maybe a
findLine predicate =
  go
  where
    go [] = Nothing
    go (value : rest)
      | predicate value = Just value
      | otherwise = go rest

diagnosticIn :: FilePath -> Diagnostic
diagnosticIn filePath =
  Diagnostic
    { diagnosticClass = DiagCompiler,
      diagnosticSeverity = Nothing,
      diagnosticReason = Nothing,
      diagnosticCode = Nothing,
      diagnosticSpan =
        RealDiagnosticSpan
          Span
            { spanFile = filePath,
              spanStartLine = 1,
              spanStartCol = 1,
              spanEndLine = 1,
              spanEndCol = 1
            },
      diagnosticMessage = T.pack "test"
    }
