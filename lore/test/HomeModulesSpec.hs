module HomeModulesSpec (spec) where

import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import Lore.Diagnostics (Diagnostic (..))
import Lore.HomeModules (LoadHomeModulesOptions (..), LoadHomeModulesResult (..), defaultLoadHomeModulesOptions)
import qualified Lore.HomeModules as HomeModules
import Lore.HomeModules.Plan
  ( HomeModuleKey (..),
    HomeModulesComponentPlan (..),
    HomeModulesLoadConfig (..),
    HomeModulesLoadInputs (..),
    HomeModulesLoadPlan (..),
    HomeModulesSelection (..),
    buildHomeModulesSelection,
    computeExternalHomeModuleDependencies,
    computeHomeModuleSourceDirs,
    homeModulesSelectionTotal,
    prepareHomeModulesComponentPlan,
    prepareHomeModulesLoadInputs,
    prepareHomeModulesLoadPlan,
  )
import Lore.TemporalModules (TemporalModule (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (fixtureLoreAt, withFixtureCopy, withFixtureSpec)

spec :: Spec
spec =
  withFixtureSpec do
    describe "home-modules planning helpers" do
      it "computeExternalHomeModuleDependencies subtracts local packages and conditionally adds directory" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          (depsWithoutTestSuite, depsWithTestSuite) <-
            fixtureLoreAt fixture fixtureRoot do
              inputs <- prepareHomeModulesLoadInputs
              let packages = inputs.homeModulesPackages
              pure
                ( computeExternalHomeModuleDependencies False packages,
                  computeExternalHomeModuleDependencies True packages
                )

          Set.member "demo-fixture" depsWithoutTestSuite `shouldBe` False
          Set.member "directory" depsWithoutTestSuite `shouldBe` False
          Set.member "directory" depsWithTestSuite `shouldBe` True

      it "prepareHomeModulesLoadPlan derives cache identity from package environment" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          cacheKey <-
            fixtureLoreAt fixture fixtureRoot do
              inputs <- prepareHomeModulesLoadInputs
              plan <- prepareHomeModulesLoadPlan inputs
              pure plan.homeModulesLoadConfig.homeModulesPackageEnvironmentCacheKey

          Set.null cacheKey `shouldBe` False
          any (isPrefixOf "package-db:") (Set.toList cacheKey) `shouldBe` True

      it "computeHomeModuleSourceDirs includes package source dirs and temporal module dirs" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          sourceDirs <-
            fixtureLoreAt fixture fixtureRoot do
              inputs <- prepareHomeModulesLoadInputs
              let packages = inputs.homeModulesPackages
              let temporalPath = fixtureRoot </> ".lore-work-test" </> "temporal-modules" </> "Temporal" </> "Sample.hs"
                  temporalModules = [TemporalModule {moduleName = GHC.mkModuleName "Temporal.Sample", modulePath = temporalPath}]
              pure (computeHomeModuleSourceDirs packages temporalModules)

          Set.member (fixtureRoot </> "src") sourceDirs `shouldBe` True
          Set.member (fixtureRoot </> ".lore-work-test" </> "temporal-modules" </> "Temporal") sourceDirs `shouldBe` True

      it "buildHomeModulesSelection keeps module and file targets separate and includes temporal modules" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          (selection, plannedFileTargets) <-
            fixtureLoreAt fixture fixtureRoot do
              dflags <- GHC.getSessionDynFlags
              inputs <- prepareHomeModulesLoadInputs
              let packages = inputs.homeModulesPackages
              componentPlan <- prepareHomeModulesComponentPlan packages
              let temporalModules =
                    [ TemporalModule
                        { moduleName = GHC.mkModuleName "Temporal.Sample",
                          modulePath = fixtureRoot </> ".lore-work-test" </> "temporal-modules" </> "Temporal" </> "Sample.hs"
                        }
                    ]
                  plannedFileTargets =
                    Set.fromList
                      [ filePath
                      | HomeModuleSourceFile filePath <- Map.keys componentPlan.homeModulesWithComponentOptions
                      ]
                  selection = buildHomeModulesSelection (GHC.homeUnitId_ dflags) componentPlan temporalModules
              pure (selection, plannedFileTargets)

          Set.member (GHC.mkModuleName "Temporal.Sample") selection.namedHomeModules `shouldBe` True
          Set.isSubsetOf plannedFileTargets selection.fileHomeModuleSources `shouldBe` True
          homeModulesSelectionTotal selection
            `shouldBe` Set.size selection.namedHomeModules + Set.size selection.fileHomeModuleSources

      it "prepareHomeModulesComponentPlan synthesizes executable Main modules into generated file targets" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          let packageFile = fixtureRoot </> "package.yaml"
              executableMainPath = fixtureRoot </> "app" </> "Main.hs"

          createDirectoryIfMissing True (fixtureRoot </> "app")
          TIO.appendFile packageFile $
            T.unlines
              [ "",
                "executables:",
                "  demo-exe:",
                "    source-dirs: app",
                "    main: Main.hs"
              ]
          TIO.writeFile executableMainPath $
            T.unlines
              [ "  module   Main (main) where",
                "main :: IO ()",
                "main = putStrLn \"demo\""
              ]

          generatedTargets <-
            fixtureLoreAt fixture fixtureRoot do
              inputs <- prepareHomeModulesLoadInputs
              componentPlan <- prepareHomeModulesComponentPlan inputs.homeModulesPackages
              pure
                [ sourcePath
                | HomeModuleSourceFile sourcePath <- Map.keys componentPlan.homeModulesWithComponentOptions,
                  "generated-main-modules" `isInfixOf` sourcePath
                ]

          case generatedTargets of
            [generatedTarget] -> do
              generatedSource <- TIO.readFile generatedTarget
              generatedTarget `shouldSatisfy` ("generated-main-modules" `isInfixOf`)
              generatedTarget `shouldNotBe` executableMainPath
              T.isInfixOf "module Main_" generatedSource `shouldBe` True
              T.isInfixOf "module Main (main) where" generatedSource `shouldBe` False
              T.isInfixOf (T.pack ("{-# LINE 1 \"" <> executableMainPath <> "\" #-}")) generatedSource `shouldBe` True
            _ ->
              expectationFailure ("Expected exactly one generated main module target, got: " <> show generatedTargets)

    describe "loadHomeModules diagnostics" do
      it "handles package definitions with no loadable components" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions

          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesTotal `shouldBe` 0
          loadResult.loadHomeModulesLoaded `shouldBe` 0
          loadResult.loadHomeModulesFailed `shouldBe` 0

      it "returns diagnostics when loading fails" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          rewriteDemo demoFile $
            T.replace
              "lookupOrZero pairs key =\n  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
              "lookupOrZero pairs key ="

          loadResult@LoadHomeModulesResult {loadHomeModulesDiagnostics} <-
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions

          loadResult.loadHomeModulesSucceeded `shouldBe` False
          loadHomeModulesDiagnostics `shouldSatisfy` (not . null)
          fmap diagnosticMessage loadHomeModulesDiagnostics
            `shouldSatisfy` any (T.isInfixOf "parse error")
          loadResult.loadHomeModulesFailed `shouldSatisfy` (> 0)

      it "MULTIPKG_LANGUAGE respects the configured default language in the multipackage workspace" \fixture -> do
        repoRoot <- makeAbsolute ".."

        loadResult <-
          fixtureLoreAt fixture repoRoot $
            HomeModules.loadHomeModules defaultLoadHomeModulesOptions

        loadResult.loadHomeModulesDiagnostics `shouldBe` []
        loadResult.loadHomeModulesSucceeded `shouldBe` True
        loadResult.loadHomeModulesLoaded `shouldBe` loadResult.loadHomeModulesTotal
        loadResult.loadHomeModulesFailed `shouldBe` 0
        loadResult.loadHomeModulesAutofixed `shouldBe` 0

    describe "loadHomeModules auto-refactor (redundant imports only)" do
      it "does not retry cleanup when auto-refactor is disabled" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions

          sourceAfter <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` False
          loadResult.loadHomeModulesAutofixed `shouldBe` 0
          sourceAfter `shouldBe` sourceBefore

      it "does not fix missing imports" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          rewriteDemo demoFile $
            T.unlines
              . filter (/= "import Data.Maybe (fromMaybe)")
              . T.lines
          sourceBefore <- TIO.readFile demoFile

          loadResult <-
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          sourceAfter <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` False
          loadResult.loadHomeModulesAutofixed `shouldBe` 0
          sourceAfter `shouldBe` sourceBefore

      it "applies redundant-import cleanup on failed load and succeeds after retry" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
              ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                  <> "import qualified Data.Sequence as Seq\n"
              )

          loadResult <-
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 1
          loadResult.loadHomeModulesAutofixedFiles `shouldBe` ["src/Demo.hs"]
          map fst loadResult.loadHomeModulesAutofixSummaryByFile `shouldBe` ["src/Demo.hs"]
          T.isInfixOf "import qualified Data.Sequence as Seq" demoSource `shouldBe` False

      it "succeeds on a second explicit load after in-loop cleanup" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          enableWarningErrors fixtureRoot
          rewriteDemo demoFile $
            T.replace
              "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
              ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                  <> "import qualified Data.Sequence as Seq\n"
              )

          firstLoad <-
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}
          secondLoad <-
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions

          firstLoad.loadHomeModulesSucceeded `shouldBe` True
          firstLoad.loadHomeModulesAutofixed `shouldBe` 1
          secondLoad.loadHomeModulesSucceeded `shouldBe` True

      it "does not clean imports when load succeeds" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          rewriteDemo demoFile $
            T.replace
              "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
              ( "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"
                  <> "import qualified Data.IntMap.Strict as IntMap\n"
              )

          loadResult <-
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 0
          T.isInfixOf "import qualified Data.IntMap.Strict as IntMap" demoSource `shouldBe` True

      it "rewrites only the targeted import and preserves neighboring import comments" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 1
          T.isInfixOf "keep-kind-comment" demoSource `shouldBe` True
          T.isInfixOf "keep-set-comment" demoSource `shouldBe` True
          T.isInfixOf "listToMaybe" demoSource `shouldBe` False
          T.isInfixOf "import Data.Maybe\n  ( fromMaybe\n  )" demoSource `shouldBe` True

      it "skips explicit-import cleanup when the import list payload contains comments" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` False
          loadResult.loadHomeModulesAutofixed `shouldBe` 0
          T.isInfixOf "keep-comment" demoSource `shouldBe` True
          T.isInfixOf "listToMaybe" demoSource `shouldBe` True

      it "removes an unused value-operator import item from a multiline qualified import list" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 1
          T.isInfixOf "(.+.)" demoSource `shouldBe` False
          T.isInfixOf "supportStep" demoSource `shouldBe` True

      it "removes an unused constructor import item while preserving needed imports from the same module" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 1
          T.isInfixOf "CtorOnly(CtorOnly)" demoSource `shouldBe` False
          T.isInfixOf "supportStep" demoSource `shouldBe` True

      it "removes an unused operator-constructor import item from a parent import" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 1
          T.isInfixOf "Op((:|))" demoSource `shouldBe` False
          T.isInfixOf "supportStep" demoSource `shouldBe` True

      it "removes an unused pattern import item from an explicit qualified import list" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 1
          T.isInfixOf "pattern SeedPattern" demoSource `shouldBe` False
          T.isInfixOf "supportSeed" demoSource `shouldBe` True

      it "removes an unused type import item declared with explicit namespace" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 1
          T.isInfixOf "import Data.Proxy" demoSource `shouldBe` False

      it "removes an unused type-operator import item declared with explicit namespace" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
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
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions {enableAutoRefactor = True}

          demoSource <- TIO.readFile demoFile
          loadResult.loadHomeModulesSucceeded `shouldBe` True
          loadResult.loadHomeModulesAutofixed `shouldBe` 1
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
