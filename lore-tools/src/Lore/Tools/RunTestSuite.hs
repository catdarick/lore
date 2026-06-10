module Lore.Tools.RunTestSuite
  ( RunTestSuiteToolOptions (..),
    runTestSuite,
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
    TestArgumentsParseError (..),
    TestSuiteComponentResult (..),
    TestSuiteComponentStatus (..),
    parseTestArguments,
    renderTestArgumentsParseError,
  )
import qualified Lore as Core
import Lore.Tools.ReloadHomeModules (renderReloadHomeModulesResult)
import Lore.Tools.Render.Diagnostics (diagnosticSummaryWithHintsDoc)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc), heading2, paragraph)
import Lore.Tools.Result
  ( ToolRun (..),
    withInterpreterSession,
  )

data RunTestSuiteToolOptions = RunTestSuiteToolOptions
  { runTestSuitePackageFilter :: Maybe Text,
    runTestSuiteRawArgs :: Maybe Text
  }
  deriving stock (Eq, Show)

runTestSuite :: (MonadLore m) => RunTestSuiteToolOptions -> m (ToolRun LoreDoc)
runTestSuite options = do
  loadResult <- Core.loadHomeModules LoadHomeModulesOptions {enableAutoRefactor = True}
  if not loadResult.loadHomeModulesSucceeded
    then ToolRunReady <$> renderReloadHomeModulesResult loadResult
    else withInterpreterSession \_ -> do
      case traverse parseTestArguments options.runTestSuiteRawArgs of
        Left parseError ->
          pure (renderParseError parseError)
        Right maybeParsedArgs -> do
          result <-
            Core.runTestSuite
              RunTestSuiteOptions
                { packageName = T.unpack <$> options.runTestSuitePackageFilter,
                  testArguments = maybe [] id maybeParsedArgs
                }
          pure $
            toLoreDoc
              RunTestSuiteOutput
                { runTestSuitePackageFilter = options.runTestSuitePackageFilter,
                  runTestSuiteForwardedArgs = result.runTestSuiteEffectiveArguments,
                  runTestSuiteComponents = result.runTestSuiteComponentResults
                }

renderParseError :: TestArgumentsParseError -> LoreDoc
renderParseError parseError =
  paragraph ("Invalid testArgs: " <> renderTestArgumentsParseError parseError <> ".")

data RunTestSuiteOutput = RunTestSuiteOutput
  { runTestSuitePackageFilter :: Maybe Text,
    runTestSuiteForwardedArgs :: [String],
    runTestSuiteComponents :: [TestSuiteComponentResult]
  }

instance ToLoreDoc RunTestSuiteOutput where
  toLoreDoc output =
    case output.runTestSuiteComponents of
      [] ->
        paragraph $
          "No test components found"
            <> packageFilterSuffix output.runTestSuitePackageFilter
            <> "."
      _ ->
        paragraph (summaryLine output)
          <> paragraph ("Forwarded arguments: " <> T.pack (show output.runTestSuiteForwardedArgs))
          <> mconcat (map componentResultDoc output.runTestSuiteComponents)

summaryLine :: RunTestSuiteOutput -> Text
summaryLine output =
  "Executed "
    <> T.pack (show (length output.runTestSuiteComponents))
    <> " test components"
    <> packageFilterSuffix output.runTestSuitePackageFilter
    <> ". Successes: "
    <> T.pack (show successCount)
    <> ", execution failures: "
    <> T.pack (show executionFailureCount)
    <> ", setup failures: "
    <> T.pack (show setupFailureCount)
    <> "."
  where
    successCount =
      length [() | TestSuiteComponentResult {status = TestSuiteComponentExecutionSuccess _} <- output.runTestSuiteComponents]
    executionFailureCount =
      length [() | TestSuiteComponentResult {status = TestSuiteComponentExecutionFailure _} <- output.runTestSuiteComponents]
    setupFailureCount =
      length [() | TestSuiteComponentResult {status = TestSuiteComponentSetupFailure _} <- output.runTestSuiteComponents]

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
