module Lore.Mcp.Tools.RunTestSuite
  ( runTestSuiteTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore
  ( MonadLore,
    RunTestSuiteOptions (..),
    TestSuiteComponentResult (..),
    TestSuiteComponentStatus (..),
    runTestSuite,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.LoreDoc (LoreDoc, ToLoreDoc (toLoreDoc), heading2, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared
  ( PartialLoadWarning,
    ToolRun,
    loadedSessionPartialWarning,
    partialLoadWarningDoc,
    withInterpreterSession,
  )
import Lore.Mcp.Tools.Shared.Diagnostics (diagnosticSummaryDoc)

data RunTestSuiteArgs (fieldType :: FieldType) = RunTestSuiteArgs
  { package ::
      Field fieldType (Maybe Text)
        `WithMeta` '[ Description "Optional package name to limit test execution. If omitted, tests from all discovered packages are executed."
                    ],
    testArgs ::
      Field fieldType (Maybe Text)
        `WithMeta` '[ Description "Optional arguments to be forwarded to the test suite.",
                      Example "--match \"some test name\""
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (RunTestSuiteArgs 'ValueType)

instance ToSchema (RunTestSuiteArgs 'MetadataType)

type RunTestSuiteResult = ToolRun RunTestSuiteOutput

data RunTestSuiteOutput = RunTestSuiteOutput
  { runTestSuitePackageFilter :: Maybe Text,
    runTestSuiteForwardedArgs :: [String],
    runTestSuiteComponents :: [TestSuiteComponentResult],
    runTestSuitePartialLoadWarning :: Maybe PartialLoadWarning
  }

instance ToLoreDoc RunTestSuiteOutput where
  toLoreDoc output =
    body <> partialLoadWarningDoc output.runTestSuitePartialLoadWarning
    where
      body =
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

runTestSuiteTool :: (MonadLore m) => SomeTool m
runTestSuiteTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "runTestSuite",
        description = Just "Runs the test suite. Equivalent to invoking 'cabal test' or 'stack test' in the terminal.",
        handler = runTestSuiteHandler
      }

runTestSuiteHandler :: (MonadLore m) => RunTestSuiteArgs 'ValueType -> m RunTestSuiteResult
runTestSuiteHandler RunTestSuiteArgs {package, testArgs} =
  withInterpreterSession \session -> do
    let parsedArgs = maybe [] (parseTestArgs . T.unpack) testArgs
    componentResults <-
      runTestSuite
        RunTestSuiteOptions
          { packageName = T.unpack <$> package,
            testArguments = parsedArgs
          }
    pure
      RunTestSuiteOutput
        { runTestSuitePackageFilter = package,
          runTestSuiteForwardedArgs = parsedArgs,
          runTestSuiteComponents = componentResults,
          runTestSuitePartialLoadWarning = loadedSessionPartialWarning session "Test execution may be incomplete."
        }

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
