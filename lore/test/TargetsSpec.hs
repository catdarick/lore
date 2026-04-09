module TargetsSpec (spec) where

import Control.Monad (void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Diagnostics (Diagnostic (..), DiagnosticClass (..), DiagnosticSpan (..), Span (..))
import Lore.Logger (LogMessage (..), LoggerHandle (..))
import Lore.Lookup (findSymbols)
import Lore.Monad (MonadLore)
import Lore.Session (defaultSessionConfig)
import qualified Lore.Session as Session
import Lore.Targets (LoadTargetsOptions (..), LoadTargetsResult (..), defaultLoadTargetsOptions, retainUnresolvedRollback)
import qualified Lore.Targets as Targets
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (fixtureLoreAt, fixtureLoreAtWithConfig, fixtureLoreAtWithLogger, withFixtureCopy)

loadTargets :: (MonadLore m) => LoadTargetsOptions -> m ()
loadTargets options = void (Targets.loadTargets options)

spec :: Spec
spec =
  do
    describe "loadTargets diagnostics" do
      it "returns final diagnostics when loading fails" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          rewriteDemo demoFile $
            T.replace
              "lookupOrZero pairs key =\n  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
              "lookupOrZero pairs key ="

          loadResult@LoadTargetsResult {loadTargetsDiagnostics} <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions

          loadResult.loadTargetsSucceeded `shouldBe` False
          loadTargetsDiagnostics `shouldSatisfy` (not . null)
          fmap diagnosticMessage loadTargetsDiagnostics
            `shouldSatisfy` any (T.isInfixOf "parse error")
          loadResult.loadTargetsModulesFailed `shouldSatisfy` (> 0)

      it "MULTIPKG_LANGUAGE respects the configured default language in the multipackage workspace" do
        repoRoot <- makeAbsolute ".."

        loadResult <-
          fixtureLoreAt repoRoot $
            Targets.loadTargets defaultLoadTargetsOptions

        loadResult.loadTargetsDiagnostics `shouldBe` []
        loadResult.loadTargetsSucceeded `shouldBe` True
        loadResult.loadTargetsModulesLoaded `shouldBe` loadResult.loadTargetsModulesTotal
        loadResult.loadTargetsModulesFailed `shouldBe` 0
        loadResult.loadTargetsModulesAutofixed `shouldBe` 0

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

      it "reports loaded, failed, and auto-fixed module counts" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          rewriteDemo demoFile $
            T.unlines
              . filter (/= "import Data.Maybe (fromMaybe)")
              . T.lines

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesLoaded `shouldBe` loadResult.loadTargetsModulesTotal
          loadResult.loadTargetsModulesFailed `shouldBe` 0
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1

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

    it "prefers customPrelude for missing-import auto-refact selection" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
            Session.SessionConfig
              { Session.projectRoot,
                Session.ghcWorkDir,
                Session.loggerHandle,
                Session.parallelWorkersLimit
              } = defaultSessionConfig
            sessionConfig =
              Session.SessionConfig
                { Session.projectRoot,
                  Session.ghcWorkDir,
                  Session.loggerHandle,
                  Session.customPrelude = Just "CustomPrelude",
                  Session.parallelWorkersLimit
                }
        ensureCustomPreludePreferenceModules fixtureRoot
        appendDemoDefinitions
          demoFile
          [ "preludePreferredValue :: Int",
            "preludePreferredValue = preludePreferred"
          ]

        loaded <- fixtureLoreAtWithConfig sessionConfig fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        loaded `shouldBe` True
        T.isInfixOf "import CustomPrelude\n" demoSource `shouldBe` True
        T.isInfixOf "import CustomPrelude (preludePreferred)\n" demoSource `shouldBe` False
        countImportHeaders "import AutoRefactFixture.Competing" demoSource `shouldBe` 0

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
          `shouldBe` Just "import AutoRefactFixture.Imports (FixtureBox(..))"

    it "adds an import for missing quoted constructors from another module" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureCrossModuleContextDataFixture fixtureRoot
        appendDemoDefinitions
          demoFile
          [ "renderContextData :: ContextData -> String",
            "renderContextData ContextData'Today = \"Today's date\"",
            "renderContextData ContextData'UserOverview = \"User overview data\""
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
        loaded `shouldBe` True
        importHeaderFor "import Schema.InternalClients.Agents.Types" demoSource
          `shouldBe` Just "import Schema.InternalClients.Agents.Types (ContextData(..))"

    it "extends an existing import list for a missing record field used via dot syntax" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        ensureRecordFieldFixtureModule fixtureRoot
        enableOverloadedRecordDot demoFile
        addImportAndKeepDefinition
          demoFile
          "import AutoRefactFixture.RecordFields (RecordBox(RecordBox))\n"
          [ "recordFieldViaDot :: RecordBox -> Int",
            "recordFieldViaDot value = value.recordField"
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
        loaded `shouldBe` True
        importHeaderFor "import AutoRefactFixture.RecordFields" demoSource
          `shouldBe` Just "import AutoRefactFixture.RecordFields (RecordBox(..))"

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

    it "skips auto-refact entirely when diagnostics are not import-fixable" do
      withFixtureCopy \fixtureRoot -> do
        let demoFile = fixtureRoot </> "src" </> "Demo.hs"
        rewriteDemo demoFile $
          T.replace
            "lookupOrZero pairs key =\n  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
            "lookupOrZero pairs key ="
        originalSource <- TIO.readFile demoFile

        logsRef <- newIORef []
        let loggerHandle =
              LoggerHandle \logMessage ->
                modifyIORef' logsRef (<> [logMessage.content])

        loaded <- fixtureLoreAtWithLogger loggerHandle fixtureRoot do
          loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          not . null <$> findSymbols "lookupOrZero"

        demoSource <- TIO.readFile demoFile
        logs <- readIORef logsRef
        loaded `shouldBe` False
        demoSource `shouldBe` originalSource
        logs `shouldSatisfy` any (== "Auto-refact: no fixable import diagnostics found; skipping.")

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
        enableImportQualifiedPost fixtureRoot
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

ensureCustomPreludePreferenceModules :: FilePath -> IO ()
ensureCustomPreludePreferenceModules fixtureRoot = do
  let srcDir = fixtureRoot </> "src"
      moduleDir = srcDir </> "AutoRefactFixture"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile (srcDir </> "CustomPrelude.hs") customPreludePreferenceModuleSource
  TIO.writeFile (moduleDir </> "Competing.hs") competingPreludePreferenceModuleSource

ensureRecordFieldFixtureModule :: FilePath -> IO ()
ensureRecordFieldFixtureModule fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "AutoRefactFixture"
      moduleFile = moduleDir </> "RecordFields.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile recordFieldFixtureModuleSource

enablePatternSynonyms :: FilePath -> IO ()
enablePatternSynonyms demoFile =
  rewriteDemo demoFile ("{-# LANGUAGE PatternSynonyms #-}\n" <>)

enableOverloadedStrings :: FilePath -> IO ()
enableOverloadedStrings demoFile =
  rewriteDemo demoFile ("{-# LANGUAGE OverloadedStrings #-}\n" <>)

enableOverloadedRecordDot :: FilePath -> IO ()
enableOverloadedRecordDot demoFile =
  rewriteDemo demoFile ("{-# LANGUAGE OverloadedRecordDot #-}\n" <>)

enableImportQualifiedPost :: FilePath -> IO ()
enableImportQualifiedPost fixtureRoot = do
  let packageFile = fixtureRoot </> "package.yaml"
  packageSource <- TIO.readFile packageFile
  TIO.writeFile packageFile $
    T.replace
      "- KindSignatures\n"
      "- KindSignatures\n- ImportQualifiedPost\n"
      packageSource

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

ensureCrossModuleContextDataFixture :: FilePath -> IO ()
ensureCrossModuleContextDataFixture fixtureRoot = do
  let schemaDir =
        fixtureRoot
          </> "src"
          </> "Schema"
          </> "InternalClients"
          </> "Agents"
  createDirectoryIfMissing True schemaDir
  TIO.writeFile (schemaDir </> "Types.hs") crossModuleContextDataTypesSource

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

recordFieldFixtureModuleSource :: T.Text
recordFieldFixtureModuleSource =
  T.unlines
    [ "module AutoRefactFixture.RecordFields",
      "  ( RecordBox(..)",
      "  )",
      "where",
      "",
      "data RecordBox = RecordBox",
      "  { recordField :: Int",
      "  }"
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

customPreludePreferenceModuleSource :: T.Text
customPreludePreferenceModuleSource =
  T.unlines
    [ "module CustomPrelude",
      "  ( module Prelude,",
      "    preludePreferred",
      "  )",
      "where",
      "",
      "import Prelude",
      "",
      "preludePreferred :: Int",
      "preludePreferred = 21"
    ]

competingPreludePreferenceModuleSource :: T.Text
competingPreludePreferenceModuleSource =
  T.unlines
    [ "module AutoRefactFixture.Competing",
      "  ( preludePreferred",
      "  )",
      "where",
      "",
      "preludePreferred :: Int",
      "preludePreferred = 7"
    ]

crossModuleContextDataTypesSource :: T.Text
crossModuleContextDataTypesSource =
  T.unlines
    [ "module Schema.InternalClients.Agents.Types",
      "  ( ContextData(..)",
      "  )",
      "where",
      "",
      "data ContextData",
      "  = ContextData'Today",
      "  | ContextData'UserOverview"
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
      diagnosticMessage = T.pack "test",
      diagnosticHints = []
    }
