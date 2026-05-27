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
    TestSuiteComponentResult (..),
    TestSuiteComponentStatus (..),
  )
import qualified Lore as Core
import Lore.Tools.ReloadHomeModules (renderReloadHomeModulesResult)
import Lore.Tools.Render.Diagnostics (diagnosticSummaryDoc)
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
      let parsedArgs = maybe [] (parseTestArgs . T.unpack) options.runTestSuiteRawArgs
      componentResults <-
        Core.runTestSuite
          RunTestSuiteOptions
            { packageName = T.unpack <$> options.runTestSuitePackageFilter,
              testArguments = parsedArgs
            }
      pure $
        toLoreDoc
          RunTestSuiteOutput
            { runTestSuitePackageFilter = options.runTestSuitePackageFilter,
              runTestSuiteForwardedArgs = parsedArgs,
              runTestSuiteComponents = componentResults
            }

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
        <> diagnosticSummaryDoc diagnostics
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

parseTestArgs :: String -> [String]
parseTestArgs raw =
  reverse (emitCurrentArg finalState.completedArgs finalState.currentArg)
  where
    finalState = foldl step (ParserState [] [] Outside) raw

    step parserState c =
      case parserState.mode of
        Outside
          | c == ' ' || c == '\t' || c == '\n' ->
              flushCurrentArg parserState
          | c == '"' ->
              parserState {mode = InDoubleQuote}
          | c == '\'' ->
              parserState {mode = InSingleQuote}
          | c == '\\' ->
              parserState {mode = Escape Outside}
          | otherwise ->
              appendChar parserState c
        InDoubleQuote
          | c == '"' ->
              parserState {mode = Outside}
          | c == '\\' ->
              parserState {mode = Escape InDoubleQuote}
          | otherwise ->
              appendChar parserState c
        InSingleQuote
          | c == '\'' ->
              parserState {mode = Outside}
          | c == '\\' ->
              parserState {mode = Escape InSingleQuote}
          | otherwise ->
              appendChar parserState c
        Escape returnMode ->
          appendChar parserState {mode = returnMode} c

    flushCurrentArg parserState@ParserState {completedArgs, currentArg} =
      parserState
        { completedArgs = emitCurrentArg completedArgs currentArg,
          currentArg = []
        }

    appendChar parserState@ParserState {currentArg} c =
      parserState {currentArg = c : currentArg}

    emitCurrentArg args current =
      if null current
        then args
        else reverse current : args

data ParserMode
  = Outside
  | InDoubleQuote
  | InSingleQuote
  | Escape ParserMode

data ParserState = ParserState
  { completedArgs :: [String],
    currentArg :: String,
    mode :: ParserMode
  }
