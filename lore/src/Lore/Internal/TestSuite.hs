module Lore.Internal.TestSuite
  ( RunTestSuiteOptions (..),
    RunTestSuiteResult (..),
    TestSuiteComponentStatus (..),
    TestSuiteComponentResult (..),
    TestArgumentsParseError (..),
    parseTestArguments,
    renderTestArgumentsParseError,
    effectiveTestArguments,
    runTestSuite,
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Data.List (intercalate, sortOn)
import qualified Data.Text as T
import qualified GHC
import Lore.Diagnostics (Diagnostic (..))
import Lore.Internal.HomeModules.EntryModules
  ( ComponentEntryModule (..),
    lookupGeneratedMainModulesByKey,
    resolveLoadedComponentEntryModule,
  )
import Lore.Internal.Interpreter (executeStatementRaw)
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries, getCachedModSummariesByFile)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Package (ComponentData (..), ComponentKind (..), PackageData (..))
import Lore.Internal.ProjectEnvironment.Types (ProjectEnvironmentState (..))
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.TestSuite.Arguments
  ( TestArgumentsParseError (..),
    parseTestArguments,
    renderTestArgumentsParseError,
  )
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.FilePath (isRelative, normalise, (</>))
import qualified UnliftIO.Directory as Dir
import qualified UnliftIO.MVar as MVar

data RunTestSuiteOptions = RunTestSuiteOptions
  { packageName :: Maybe String,
    testArguments :: [String]
  }

data RunTestSuiteResult = RunTestSuiteResult
  { runTestSuiteEffectiveArguments :: [String],
    runTestSuiteComponentResults :: [TestSuiteComponentResult]
  }
  deriving stock (Eq, Show)

data TestSuiteComponentStatus
  = TestSuiteComponentSetupFailure String
  | TestSuiteComponentExecutionFailure [Diagnostic]
  | TestSuiteComponentExecutionSuccess String
  deriving stock (Eq, Show)

data TestSuiteComponentResult = TestSuiteComponentResult
  { packageName :: String,
    componentName :: String,
    moduleName :: Maybe String,
    status :: TestSuiteComponentStatus
  }
  deriving stock (Eq, Show)

runTestSuite :: (MonadLore m) => RunTestSuiteOptions -> m RunTestSuiteResult
runTestSuite options@RunTestSuiteOptions {packageName = packageFilter} = do
  sessionProjectRoot <- asks projectRoot
  defaultArguments <- asks testSuiteDefaultArguments
  absoluteSessionProjectRoot <- liftIO (Dir.makeAbsolute sessionProjectRoot)
  maybeProjectEnvironment <- asks projectEnvironmentStateVar >>= liftIO . MVar.readMVar
  case maybeProjectEnvironment of
    Nothing ->
      pure
        RunTestSuiteResult
          { runTestSuiteEffectiveArguments = effectiveTestArguments defaultArguments options,
            runTestSuiteComponentResults =
              [ TestSuiteComponentResult
                  { packageName = maybe "<all-packages>" id packageFilter,
                    componentName = "test-suite",
                    moduleName = Nothing,
                    status = TestSuiteComponentSetupFailure "No successfully loaded project environment is available. Run reloadHomeModules before runTestSuite."
                  }
              ]
          }
    Just projectEnvironment -> runTestSuiteWithPackages options sessionProjectRoot absoluteSessionProjectRoot defaultArguments projectEnvironment.projectEnvironmentPackages

runTestSuiteWithPackages :: (MonadLore m) => RunTestSuiteOptions -> FilePath -> FilePath -> [String] -> [PackageData] -> m RunTestSuiteResult
runTestSuiteWithPackages options@RunTestSuiteOptions {packageName = packageFilter} _sessionProjectRoot absoluteSessionProjectRoot defaultArguments packages = do
  generatedMainModulesByKey <- lookupGeneratedMainModulesByKey
  modSummariesByFile <- getCachedModSummariesByFile
  ModSummaries modSummariesByModule <- getCachedModSummaries
  let testComponents =
        [ (pkg.packageName, pkg.packageRoot, component)
        | pkg <- sortOn (.packageName) packages,
          packageMatches packageFilter pkg.packageName,
          component <- sortOn (.componentName) pkg.components,
          component.componentKind == ComponentKindTest
        ]
      effectiveArguments = effectiveTestArguments defaultArguments options
  componentResults <- forM testComponents \(pkgName, pkgRoot, component) -> do
    resolvedEntryModule <-
      resolveLoadedComponentEntryModule
        pkgName
        pkgRoot
        component
        generatedMainModulesByKey
        modSummariesByFile
        modSummariesByModule
    case resolvedEntryModule of
      Left reason ->
        pure
          TestSuiteComponentResult
            { packageName = pkgName,
              componentName = component.componentName,
              moduleName = Nothing,
              status = TestSuiteComponentSetupFailure reason
            }
      Right ComponentEntryModule {entryModule} -> do
        let entryModuleName =
              GHC.moduleNameString (GHC.moduleName entryModule)
            executionDir = resolveExecutionDir absoluteSessionProjectRoot pkgRoot
            statement = renderRunStatement executionDir entryModuleName effectiveArguments
        runResult <- executeStatementRaw (T.pack statement)
        componentStatus <-
          case runResult of
            Left runDiagnostics -> do
              logExecutionFailure pkgName component.componentName entryModuleName runDiagnostics
              pure (TestSuiteComponentExecutionFailure runDiagnostics)
            Right renderedOutput ->
              pure (TestSuiteComponentExecutionSuccess renderedOutput)
        pure
          TestSuiteComponentResult
            { packageName = pkgName,
              componentName = component.componentName,
              moduleName = Just entryModuleName,
              status = componentStatus
            }
  pure
    RunTestSuiteResult
      { runTestSuiteEffectiveArguments = effectiveArguments,
        runTestSuiteComponentResults = componentResults
      }

effectiveTestArguments :: [String] -> RunTestSuiteOptions -> [String]
effectiveTestArguments defaultArguments RunTestSuiteOptions {testArguments} =
  defaultArguments <> testArguments

packageMatches :: Maybe String -> String -> Bool
packageMatches maybePackageName packageName =
  case maybePackageName of
    Nothing -> True
    Just expectedPackageName -> expectedPackageName == packageName

renderRunStatement :: FilePath -> String -> [String] -> String
renderRunStatement executionDir entryModuleName args =
  -- CWD must be switched inside the interpreted statement because execStmt runs
  -- in the external interpreter process, not in the host process running lore.
  "System.Directory.withCurrentDirectory "
    <> show executionDir
    <> " (System.Environment.withArgs "
    <> show args
    <> " "
    <> entryModuleName
    <> ".main)"

resolveExecutionDir :: FilePath -> FilePath -> FilePath
resolveExecutionDir absoluteSessionProjectRoot packageRoot
  | isRelative packageRoot = normalise (absoluteSessionProjectRoot </> packageRoot)
  | otherwise = normalise packageRoot

logExecutionFailure :: (MonadLore m) => String -> String -> String -> [Diagnostic] -> m ()
logExecutionFailure packageName componentName moduleName diagnostics =
  Log.err $
    intercalate "\n" $
      ("Test suite execution failed for " <> packageName <> "/" <> componentName <> " (" <> moduleName <> "):")
        : map renderDiagnostic diagnostics
  where
    renderDiagnostic Diagnostic {diagnosticMessage, diagnosticReason, diagnosticHints} =
      "- " <> intercalate " | " (messageField : reasonField <> hintsField)
      where
        messageField = "message=" <> renderText diagnosticMessage
        reasonField =
          case diagnosticReason of
            Nothing -> []
            Just reasonText -> ["reason=" <> renderText reasonText]
        hintsField =
          case diagnosticHints of
            [] -> []
            hints -> ["hints=" <> intercalate " || " (map renderText hints)]

    renderText =
      T.unpack . T.replace "\r" "\\r" . T.replace "\n" "\\n"
