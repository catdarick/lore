module TargetsSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Diagnostics (Diagnostic (..))
import Lore.Targets (LoadTargetsOptions (..), LoadTargetsResult (..), defaultLoadTargetsOptions)
import qualified Lore.Targets as Targets
import System.Directory (doesFileExist, makeAbsolute, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (fixtureLoreAt, withFixtureCopy)

spec :: Spec
spec =
  do
    describe "loadTargets diagnostics" do
      it "handles package definitions with no loadable components" do
        withFixtureCopy \fixtureRoot -> do
          let packageFile = fixtureRoot </> "package.yaml"
              fixtureCabalFile = fixtureRoot </> "demo-fixture.cabal"
          fixtureCabalExists <- doesFileExist fixtureCabalFile
          if fixtureCabalExists
            then removeFile fixtureCabalFile
            else pure ()
          TIO.writeFile packageFile $
            T.unlines
              [ "name: demo",
                "version: 0.1.0.0",
                "dependencies:",
                "- base >= 4.7 && < 5"
              ]

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions

          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesTotal `shouldBe` 0
          loadResult.loadTargetsModulesLoaded `shouldBe` 0
          loadResult.loadTargetsModulesFailed `shouldBe` 0

      it "returns diagnostics when loading fails" do
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

    describe "loadTargets auto-refact (redundant imports only)" do
      it "does not retry cleanup when auto-refactor is disabled" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
              ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                  <> "import qualified Data.Sequence as Seq\n"
              )
          sourceBefore <- TIO.readFile demoFile

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions

          sourceAfter <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` False
          loadResult.loadTargetsModulesAutofixed `shouldBe` 0
          sourceAfter `shouldBe` sourceBefore

      it "does not fix missing imports" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          rewriteDemo demoFile $
            T.unlines
              . filter (/= "import Data.Maybe (fromMaybe)")
              . T.lines
          sourceBefore <- TIO.readFile demoFile

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          sourceAfter <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` False
          loadResult.loadTargetsModulesAutofixed `shouldBe` 0
          sourceAfter `shouldBe` sourceBefore

      it "applies redundant-import cleanup on failed load and succeeds after retry" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
              ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                  <> "import qualified Data.Sequence as Seq\n"
              )

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1
          loadResult.loadTargetsAutofixedFiles `shouldBe` ["src/Demo.hs"]
          map fst loadResult.loadTargetsAutofixSummaryByFile `shouldBe` ["src/Demo.hs"]
          T.isInfixOf "import qualified Data.Sequence as Seq" demoSource `shouldBe` False

      it "succeeds on a second explicit load after in-loop cleanup" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
              ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                  <> "import qualified Data.Sequence as Seq\n"
              )

          firstLoad <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}
          secondLoad <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions

          firstLoad.loadTargetsSucceeded `shouldBe` True
          firstLoad.loadTargetsModulesAutofixed `shouldBe` 1
          secondLoad.loadTargetsSucceeded `shouldBe` True

      it "does not clean imports when load succeeds" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          rewriteDemo demoFile $
            T.replace
              "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
              ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                  <> "import qualified Data.IntMap.Strict as IntMap\n"
              )

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 0
          T.isInfixOf "import qualified Data.IntMap.Strict as IntMap" demoSource `shouldBe` True

      it "rewrites only the targeted import and preserves neighboring import comments" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import Data.Kind (Type)\n"
              "import Data.Kind (Type) -- keep-kind-comment\n"
              . T.replace
                "import Data.Maybe (fromMaybe)\n"
                ( T.unlines
                    [ "import Data.Maybe",
                      "  ( fromMaybe,",
                      "    maybe,",
                      "    listToMaybe",
                      "  )"
                    ]
                )
              . T.replace
                "import qualified Data.Set as Set (Set, fromList, member)\n"
                "import qualified Data.Set as Set (Set, fromList, member) -- keep-set-comment\n"

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1
          T.isInfixOf "keep-kind-comment" demoSource `shouldBe` True
          T.isInfixOf "keep-set-comment" demoSource `shouldBe` True
          T.isInfixOf "listToMaybe" demoSource `shouldBe` False
          T.isInfixOf "import Data.Maybe\n  ( fromMaybe\n  )" demoSource `shouldBe` True

      it "skips explicit-import cleanup when the import list payload contains comments" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import Data.Maybe (fromMaybe)\n"
              ( T.unlines
                  [ "import Data.Maybe",
                    "  ( fromMaybe, -- keep-comment",
                    "    maybe,",
                    "    listToMaybe",
                    "  )"
                  ]
              )

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` False
          loadResult.loadTargetsModulesAutofixed `shouldBe` 0
          T.isInfixOf "keep-comment" demoSource `shouldBe` True
          T.isInfixOf "listToMaybe" demoSource `shouldBe` True

      it "removes an unused value-operator import item from a multiline qualified import list" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            replaceSupportImport
              ( T.unlines
                  [ "import qualified Demo.Support as Support",
                    "  ( SupportRecord,",
                    "    mkSupportRecord,",
                    "    supportSeed,",
                    "    supportStep,",
                    "    (.+.)",
                    "  )"
                  ]
              )

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1
          T.isInfixOf "(.+.)" demoSource `shouldBe` False
          T.isInfixOf "supportStep" demoSource `shouldBe` True

      it "removes an unused constructor import item while preserving needed imports from the same module" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
              supportFile = fixtureRoot </> "src" </> "Demo" </> "Support.hs"
          enableWarningErrors fixtureRoot
          rewriteSupport supportFile $
            appendSupportDefinition
              "data CtorOnly = CtorOnly\n"
              . addSupportExportItem "CtorOnly(CtorOnly)"
          rewriteDemo demoFile $
            replaceSupportImport
              "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep, CtorOnly(CtorOnly))\n"

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1
          T.isInfixOf "CtorOnly(CtorOnly)" demoSource `shouldBe` False
          T.isInfixOf "supportStep" demoSource `shouldBe` True

      it "removes an unused operator-constructor import item from a parent import" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
              supportFile = fixtureRoot </> "src" </> "Demo" </> "Support.hs"
          enableWarningErrors fixtureRoot
          rewriteSupport supportFile $
            appendSupportDefinition
              "data Op a = a :| a\n"
              . addSupportExportItem "Op((:|))"
          rewriteDemo demoFile $
            addModulePragma "{-# LANGUAGE TypeOperators #-}\n"
              . replaceSupportImport
                "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep, Op((:|)))\n"

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1
          T.isInfixOf "Op((:|))" demoSource `shouldBe` False
          T.isInfixOf "supportStep" demoSource `shouldBe` True

      it "removes an unused pattern import item from an explicit qualified import list" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
              supportFile = fixtureRoot </> "src" </> "Demo" </> "Support.hs"
          enableWarningErrors fixtureRoot
          rewriteSupport supportFile $
            addModulePragma "{-# LANGUAGE PatternSynonyms #-}\n"
              . appendSupportDefinition
                ( T.unlines
                    [ "pattern SeedPattern :: Int",
                      "pattern SeedPattern = 5"
                    ]
                )
              . addSupportExportItem "pattern SeedPattern"
          rewriteDemo demoFile $
            addModulePragma "{-# LANGUAGE PatternSynonyms #-}\n"
              . replaceSupportImport
                "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep, pattern SeedPattern)\n"

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1
          T.isInfixOf "pattern SeedPattern" demoSource `shouldBe` False
          T.isInfixOf "supportSeed" demoSource `shouldBe` True

      it "removes an unused type import item declared with explicit namespace" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import Data.Kind (Type)\n"
              ( T.unlines
                  [ "import Data.Kind (Type)",
                    "import Data.Proxy (type Proxy)"
                  ]
              )

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1
          T.isInfixOf "import Data.Proxy" demoSource `shouldBe` False

      it "removes an unused type-operator import item declared with explicit namespace" do
        withFixtureCopy \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import Data.Kind (Type)\n"
              ( T.unlines
                  [ "import Data.Kind (Type)",
                    "import GHC.TypeNats (type (+))"
                  ]
              )

          loadResult <-
            fixtureLoreAt fixtureRoot $
              Targets.loadTargets defaultLoadTargetsOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadTargetsSucceeded `shouldBe` True
          loadResult.loadTargetsModulesAutofixed `shouldBe` 1
          T.isInfixOf "import GHC.TypeNats" demoSource `shouldBe` False

rewriteDemo :: FilePath -> (T.Text -> T.Text) -> IO ()
rewriteDemo demoFile rewrite =
  TIO.readFile demoFile >>= TIO.writeFile demoFile . rewrite

rewriteSupport :: FilePath -> (T.Text -> T.Text) -> IO ()
rewriteSupport supportFile rewrite =
  TIO.readFile supportFile >>= TIO.writeFile supportFile . rewrite

replaceSupportImport :: T.Text -> T.Text -> T.Text
replaceSupportImport replacement =
  T.replace
    "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
    replacement

addSupportExportItem :: T.Text -> T.Text -> T.Text
addSupportExportItem exportItem =
  T.replace
    "    (.+.),\n  )"
    ("    " <> exportItem <> ",\n    (.+.),\n  )")

appendSupportDefinition :: T.Text -> T.Text -> T.Text
appendSupportDefinition definition source =
  if T.isInfixOf definition source
    then source
    else source <> "\n" <> definition

addModulePragma :: T.Text -> T.Text -> T.Text
addModulePragma pragma source =
  if T.isPrefixOf pragma source
    then source
    else pragma <> source

enableWarningErrors :: FilePath -> IO ()
enableWarningErrors fixtureRoot = do
  let packageFile = fixtureRoot </> "package.yaml"
  packageSource <- TIO.readFile packageFile
  let warningErrorsBlock =
        T.unlines
          [ "ghc-options:",
            "- -Werror",
            "- -Wunused-imports",
            ""
          ]
      withInsertedOptions =
        T.replace
          "library:\n"
          (warningErrorsBlock <> "library:\n")
          packageSource
  TIO.writeFile packageFile $
    if withInsertedOptions == packageSource
      then warningErrorsBlock <> packageSource
      else withInsertedOptions
