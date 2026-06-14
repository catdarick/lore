module Lore.Tools.RunTestSuite
  ( RunTestSuiteToolOptions (..),
    RunTestSuiteOutcome (..),
    RunTestSuiteExecution (..),
    RunTestSuiteStatus (..),
    RunTestSuiteExecutionStatus (..),
    RunTestSuiteExecutionSummary (..),
    runTestSuite,
    runTestSuiteStatus,
    runTestSuiteExecutionStatus,
    runTestSuiteExecutionSummary,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore
  ( LoadHomeModulesOptions (..),
    LoadHomeModulesResult (..),
    MonadLore,
    RunTestSuiteOptions (..),
    RunTestSuiteResult (..),
    TestArgumentsParseError,
    TestSuiteComponentResult (..),
    TestSuiteComponentStatus (..),
    parseTestArguments,
    projectEnvironmentFailureRequiresRestart,
    renderTestArgumentsParseError,
  )
import qualified Lore as Core
import Lore.Tools.Render.Diagnostics (diagnosticSummaryWithHintsDoc)
import Lore.Tools.Render.Doc (LoreDoc, heading2, paragraph)
import Lore.Tools.ReloadHomeModules (renderReloadHomeModulesResult)
import Lore.Tools.Result
  ( RenderedResult (..),
    ToolBlocked (..),
    ToolRun (..),
    withInterpreterSession,
  )

data RunTestSuiteToolOptions = RunTestSuiteToolOptions
  { runTestSuitePackageFilter :: Maybe Text,
    runTestSuiteRawArgs :: Maybe Text
  }
  deriving stock (Eq, Show)

data RunTestSuiteOutcome
  = RunTestSuiteLoadFailed LoadHomeModulesResult
  | RunTestSuiteInvalidArguments Text
  | RunTestSuiteExecuted RunTestSuiteExecution
  deriving stock (Eq, Show)

data RunTestSuiteExecution = RunTestSuiteExecution
  { runTestSuitePackageFilter :: Maybe Text,
    runTestSuiteEffectiveArguments :: [String],
    runTestSuiteComponents :: [TestSuiteComponentResult]
  }
  deriving stock (Eq, Show)

data RunTestSuiteExecutionSummary = RunTestSuiteExecutionSummary
  { runTestSuiteTotalComponents :: Int,
    runTestSuitePassedComponents :: Int,
    runTestSuiteSetupFailures :: Int,
    runTestSuiteExecutionFailures :: Int
  }
  deriving stock (Eq, Show)

data RunTestSuiteExecutionStatus
  = RunTestSuiteExecutionNoTests
  | RunTestSuiteExecutionPassed
  | RunTestSuiteExecutionFailed
  deriving stock (Eq, Show)

data RunTestSuiteStatus
  = RunTestSuiteStatusCompilationFailure
  | RunTestSuiteStatusEnvironmentFailure
  | RunTestSuiteStatusRestartRequired
  | RunTestSuiteStatusInvalidArguments
  | RunTestSuiteStatusNoTests
  | RunTestSuiteStatusTestsPassed
  | RunTestSuiteStatusTestsFailed
  | RunTestSuiteStatusBlocked
  deriving stock (Eq, Show)

runTestSuite :: (MonadLore m) => RunTestSuiteToolOptions -> m (ToolRun (RenderedResult RunTestSuiteOutcome))
runTestSuite options = do
  loadResult <- Core.loadHomeModules LoadHomeModulesOptions {enableAutoRefactor = True}
  case loadResult of
    LoadHomeModulesCompleted summary
      | summary.homeModulesCompilationSucceeded -> withInterpreterSession \_ ->
          case traverse parseTestArguments options.runTestSuiteRawArgs of
            Left parseError ->
              pure (renderInvalidArguments parseError)
            Right maybeParsedArgs -> do
              result <-
                Core.runTestSuite
                  RunTestSuiteOptions
                    { packageName = T.unpack <$> options.runTestSuitePackageFilter,
                      testArguments = maybe [] id maybeParsedArgs
                    }
              pure $
                renderExecuted
                  RunTestSuiteExecution
                    { runTestSuitePackageFilter = options.runTestSuitePackageFilter,
                      runTestSuiteEffectiveArguments = result.runTestSuiteEffectiveArguments,
                      runTestSuiteComponents = result.runTestSuiteComponentResults
                    }
    _ -> ToolRunReady <$> renderLoadFailure loadResult

runTestSuiteStatus :: ToolRun (RenderedResult RunTestSuiteOutcome) -> RunTestSuiteStatus
runTestSuiteStatus = \case
  ToolRunBlocked InterpreterContextNotReady ->
    RunTestSuiteStatusBlocked
  ToolRunReady renderedResult ->
    case renderedResult.renderedResultValue of
      RunTestSuiteLoadFailed loadResult ->
        case loadResult of
          LoadHomeModulesCompleted _ -> RunTestSuiteStatusCompilationFailure
          LoadHomeModulesPreparationFailed failure
            | projectEnvironmentFailureRequiresRestart failure -> RunTestSuiteStatusRestartRequired
            | otherwise -> RunTestSuiteStatusEnvironmentFailure
      RunTestSuiteInvalidArguments {} ->
        RunTestSuiteStatusInvalidArguments
      RunTestSuiteExecuted execution ->
        case runTestSuiteExecutionStatus execution of
          RunTestSuiteExecutionNoTests ->
            RunTestSuiteStatusNoTests
          RunTestSuiteExecutionPassed ->
            RunTestSuiteStatusTestsPassed
          RunTestSuiteExecutionFailed ->
            RunTestSuiteStatusTestsFailed

runTestSuiteExecutionStatus :: RunTestSuiteExecution -> RunTestSuiteExecutionStatus
runTestSuiteExecutionStatus execution
  | summary.runTestSuiteTotalComponents == 0 =
      RunTestSuiteExecutionNoTests
  | summary.runTestSuiteSetupFailures > 0 || summary.runTestSuiteExecutionFailures > 0 =
      RunTestSuiteExecutionFailed
  | otherwise =
      RunTestSuiteExecutionPassed
  where
    summary = runTestSuiteExecutionSummary execution

runTestSuiteExecutionSummary :: RunTestSuiteExecution -> RunTestSuiteExecutionSummary
runTestSuiteExecutionSummary execution =
  RunTestSuiteExecutionSummary
    { runTestSuiteTotalComponents = length execution.runTestSuiteComponents,
      runTestSuitePassedComponents =
        length
          [ ()
          | TestSuiteComponentResult
              { status = TestSuiteComponentExecutionSuccess _
              } <-
              execution.runTestSuiteComponents
          ],
      runTestSuiteSetupFailures =
        length
          [ ()
          | TestSuiteComponentResult
              { status = TestSuiteComponentSetupFailure _
              } <-
              execution.runTestSuiteComponents
          ],
      runTestSuiteExecutionFailures =
        length
          [ ()
          | TestSuiteComponentResult
              { status = TestSuiteComponentExecutionFailure _
              } <-
              execution.runTestSuiteComponents
          ]
    }

renderLoadFailure :: (MonadLore m) => LoadHomeModulesResult -> m (RenderedResult RunTestSuiteOutcome)
renderLoadFailure loadResult = do
  document <- renderReloadHomeModulesResult loadResult
  pure
    RenderedResult
      { renderedResultValue = RunTestSuiteLoadFailed loadResult,
        renderedResultDocument = document
      }

renderInvalidArguments :: TestArgumentsParseError -> RenderedResult RunTestSuiteOutcome
renderInvalidArguments parseError =
  let message = renderTestArgumentsParseError parseError
   in RenderedResult
        { renderedResultValue = RunTestSuiteInvalidArguments message,
          renderedResultDocument = paragraph ("Invalid testArgs: " <> message <> ".")
        }

renderExecuted :: RunTestSuiteExecution -> RenderedResult RunTestSuiteOutcome
renderExecuted execution =
  RenderedResult
    { renderedResultValue = RunTestSuiteExecuted execution,
      renderedResultDocument =
        case execution.runTestSuiteComponents of
          [] ->
            paragraph $
              "No test components found"
                <> packageFilterSuffix execution.runTestSuitePackageFilter
                <> "."
          _ ->
            paragraph (summaryLine execution)
              <> paragraph ("Forwarded arguments: " <> T.pack (show execution.runTestSuiteEffectiveArguments))
              <> mconcat (map componentResultDoc execution.runTestSuiteComponents)
    }

summaryLine :: RunTestSuiteExecution -> Text
summaryLine execution =
  "Executed "
    <> T.pack (show summary.runTestSuiteTotalComponents)
    <> " test components"
    <> packageFilterSuffix execution.runTestSuitePackageFilter
    <> ". Successes: "
    <> T.pack (show summary.runTestSuitePassedComponents)
    <> ", execution failures: "
    <> T.pack (show summary.runTestSuiteExecutionFailures)
    <> ", setup failures: "
    <> T.pack (show summary.runTestSuiteSetupFailures)
    <> "."
  where
    summary = runTestSuiteExecutionSummary execution

componentResultDoc :: TestSuiteComponentResult -> LoreDoc
componentResultDoc TestSuiteComponentResult {packageName, componentName, moduleName, status} =
  case status of
    TestSuiteComponentSetupFailure failureReason ->
      heading2 ("[SETUP FAIL] " <> componentLabel)
        <> paragraph ("reason: " <> T.pack failureReason)
    TestSuiteComponentExecutionFailure diagnostics ->
      heading2 ("[FAIL] " <> componentLabel)
        <> diagnosticSummaryWithHintsDoc diagnostics
    TestSuiteComponentExecutionSuccess output ->
      heading2 ("[PASS] " <> componentLabel)
        <> paragraph
          ( if null output
              then "output: <empty>"
              else T.pack output
          )
  where
    componentLabel =
      T.pack packageName
        <> "/"
        <> T.pack componentName
        <> maybe "" (\name -> " (" <> T.pack name <> ")") moduleName

packageFilterSuffix :: Maybe Text -> Text
packageFilterSuffix maybePackage =
  case maybePackage of
    Nothing -> ""
    Just packageName -> " for package " <> T.pack (show (T.unpack packageName))
