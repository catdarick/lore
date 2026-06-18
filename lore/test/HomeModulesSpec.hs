module HomeModulesSpec (spec) where

import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import Lore.Diagnostics (Diagnostic (..))
import Lore.HomeModules (HomeModulesLoadSummary (..), LoadHomeModulesOptions (..), LoadHomeModulesResult (..), defaultLoadHomeModulesOptions)
import qualified Lore.HomeModules as HomeModules
import Lore.HomeModules.Plan
  ( HomeModuleKey (..),
    HomeModulesComponentPlan (..),
    HomeModulesLoadConfig (..),
    HomeModulesLoadInputs (..),
    HomeModulesLoadPlan (..),
    HomeModulesSelection (..),
    buildHomeModulesSelection,
    computeHomeModuleSourceDirs,
    homeModulesSelectionTotal,
    prepareHomeModulesComponentPlan,
    prepareHomeModulesLoadPlan,
  )
import Lore.Internal.HomeModules.Plan (prepareHomeModulesLoadInputsFromProjectEnvironment)
import Lore.Internal.ProjectEnvironment.Prepare (prepareProjectDescription)
import Lore.Internal.ProjectEnvironment.Refresh (refreshProjectEnvironment)
import Lore.Internal.ProjectEnvironment.Types (PreparedProjectDescription (..), ProjectEnvironmentRefresh (..))
import Lore.Monad (MonadLore)
import Lore.Session (ParallelWorkersCount (..), SessionConfig (..), defaultSessionConfig)
import Lore.TemporalModules (TemporalModule (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute, removeFile)
import System.FilePath ((</>))
import qualified System.Timeout as Timeout
import Test.Hspec
import TestSupport (fixtureLoreAt, fixtureLoreAtWithConfig, withFixtureCopy, withFixtureSpec)

spec :: Spec
spec =
  withFixtureSpec do
    describe "home-modules planning helpers" do
      it "prepareProjectDescription derives required external dependencies from prepared packages" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          dependencies <-
            fixtureLoreAt fixture fixtureRoot preparedRequiredExternalDependenciesForTest

          Set.member "demo-fixture" dependencies `shouldBe` False
          Set.member "directory" dependencies `shouldBe` False
          Set.member "base" dependencies `shouldBe` True
          Set.member "containers" dependencies `shouldBe` True

      it "prepareHomeModulesLoadPlan derives cache identity from package environment" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          cacheKey <-
            fixtureLoreAt fixture fixtureRoot do
              inputs <- prepareLoadInputsForTest
              plan <- prepareHomeModulesLoadPlan inputs
              pure plan.homeModulesLoadConfig.homeModulesPackageEnvironmentCacheKey

          Set.null cacheKey `shouldBe` False
          any (isPrefixOf "package-db:") (Set.toList cacheKey) `shouldBe` True

      it "computeHomeModuleSourceDirs includes package source dirs and temporal module dirs" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          sourceDirs <-
            fixtureLoreAt fixture fixtureRoot do
              inputs <- prepareLoadInputsForTest
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
              inputs <- prepareLoadInputsForTest
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
              inputs <- prepareLoadInputsForTest
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

          loadSucceeded loadResult `shouldBe` True
          loadTotal loadResult `shouldBe` 0
          loadLoaded loadResult `shouldBe` 0
          loadFailed loadResult `shouldBe` 0

      it "returns diagnostics when loading fails" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          let demoFile = fixtureRoot </> "src" </> "Demo.hs"
          rewriteDemo demoFile $
            T.replace
              "lookupOrZero pairs key =\n  fromMaybe 0 (Map.lookup key (Map.fromList pairs))"
              "lookupOrZero pairs key ="

          loadResult <-
            fixtureLoreAt fixture fixtureRoot $
              HomeModules.loadHomeModules defaultLoadHomeModulesOptions
          let loadDiagnostics = loadDiagnosticsOf loadResult

          loadSucceeded loadResult `shouldBe` False
          loadDiagnostics `shouldSatisfy` (not . null)
          fmap diagnosticMessage loadDiagnostics
            `shouldSatisfy` any (T.isInfixOf "parse error")
          loadFailed loadResult `shouldSatisfy` (> 0)

      it "MULTIPKG_LANGUAGE loads the dependent multipackage workspace with two parallel workers" \fixture -> do
        repoRoot <- makeAbsolute ".."

        maybeLoadResult <-
          Timeout.timeout 30_000_000 $
            fixtureLoreAtWithConfig
              fixture
              defaultSessionConfig {parallelWorkersLimit = ThisWorkersCount 2}
              repoRoot
              (HomeModules.loadHomeModules defaultLoadHomeModulesOptions)

        case maybeLoadResult of
          Nothing ->
            expectationFailure "Parallel multipackage home-module loading timed out"
          Just loadResult -> do
            loadDiagnosticsOf loadResult `shouldBe` []
            loadSucceeded loadResult `shouldBe` True
            loadLoaded loadResult `shouldBe` loadTotal loadResult
            loadFailed loadResult `shouldBe` 0
            loadAutofixed loadResult `shouldBe` 0

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
          loadSucceeded loadResult `shouldBe` False
          loadAutofixed loadResult `shouldBe` 0
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
          loadSucceeded loadResult `shouldBe` False
          loadAutofixed loadResult `shouldBe` 0
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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 1
          loadAutofixedFiles loadResult `shouldBe` ["src/Demo.hs"]
          map fst (loadAutofixSummaryByFile loadResult) `shouldBe` ["src/Demo.hs"]
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

          loadSucceeded firstLoad `shouldBe` True
          loadAutofixed firstLoad `shouldBe` 1
          loadSucceeded secondLoad `shouldBe` True

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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 0
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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 1
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
          loadSucceeded loadResult `shouldBe` False
          loadAutofixed loadResult `shouldBe` 0
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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 1
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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 1
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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 1
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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 1
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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 1
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
          loadSucceeded loadResult `shouldBe` True
          loadAutofixed loadResult `shouldBe` 1
          T.isInfixOf "import GHC.TypeNats" demoSource `shouldBe` False

prepareLoadInputsForTest :: (MonadLore m) => m HomeModulesLoadInputs
prepareLoadInputsForTest = do
  refreshResult <- refreshProjectEnvironment
  case refreshResult of
    Left failure ->
      error ("Failed to prepare project environment in test: " <> show failure)
    Right refresh ->
      prepareHomeModulesLoadInputsFromProjectEnvironment
        refresh.refreshedProjectEnvironment

preparedRequiredExternalDependenciesForTest :: (MonadLore m) => m (Set.Set String)
preparedRequiredExternalDependenciesForTest = do
  descriptionResult <- prepareProjectDescription
  case descriptionResult of
    Left failure ->
      error ("Failed to prepare project description in test: " <> show failure)
    Right prepared ->
      pure prepared.preparedRequiredExternalDependencies

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

loadSummaryOf :: LoadHomeModulesResult -> HomeModulesLoadSummary
loadSummaryOf (LoadHomeModulesCompleted summary) = summary
loadSummaryOf (LoadHomeModulesPreparationFailed failure) = error ("Expected completed load, got preparation failure: " <> show failure)

loadSucceeded :: LoadHomeModulesResult -> Bool
loadSucceeded = (.homeModulesCompilationSucceeded) . loadSummaryOf

loadDiagnosticsOf :: LoadHomeModulesResult -> [Diagnostic]
loadDiagnosticsOf = (.homeModulesDiagnostics) . loadSummaryOf

loadLoaded :: LoadHomeModulesResult -> Int
loadLoaded = (.homeModulesLoaded) . loadSummaryOf

loadFailed :: LoadHomeModulesResult -> Int
loadFailed = (.homeModulesFailed) . loadSummaryOf

loadAutofixed :: LoadHomeModulesResult -> Int
loadAutofixed = (.homeModulesAutofixed) . loadSummaryOf

loadAutofixedFiles :: LoadHomeModulesResult -> [FilePath]
loadAutofixedFiles = (.homeModulesAutofixedFiles) . loadSummaryOf

loadAutofixSummaryByFile :: LoadHomeModulesResult -> [(FilePath, [String])]
loadAutofixSummaryByFile = (.homeModulesAutofixSummaryByFile) . loadSummaryOf

loadTotal :: LoadHomeModulesResult -> Int
loadTotal = (.homeModulesTotal) . loadSummaryOf
