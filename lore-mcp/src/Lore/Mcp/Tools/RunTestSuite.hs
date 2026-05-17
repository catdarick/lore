module Lore.Mcp.Tools.RunTestSuite
  ( runTestSuiteTool,
  )
where

import qualified Data.Aeson as J
import Data.List (intercalate)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore
  ( MonadLore,
    RunTestSuiteOptions (..),
    TestSuiteComponentResult (..),
    TestSuiteComponentStatus (..),
    interpreterContextIsReady,
    lookupLastLoadTargetsResult,
    runTestSuite,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (appendPartialLoadWarning)
import Lore.Mcp.Tools.Shared.Diagnostics (renderDiagnosticSummary)

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

runTestSuiteTool :: (MonadLore m) => SomeTool m
runTestSuiteTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "runTestSuite",
        description = Just "Runs the test suite. Equivalent to invoking 'cabal test' or 'stack test' in the terminal.",
        handler = runTestSuiteHandler
      }

runTestSuiteHandler :: (MonadLore m) => RunTestSuiteArgs 'ValueType -> m Text
runTestSuiteHandler RunTestSuiteArgs {package, testArgs} = do
  maybeLoadResult <- lookupLastLoadTargetsResult
  contextReady <- interpreterContextIsReady
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult
      | not contextReady ->
          pure "Interpreter context is not ready. Run reloadHomeModules again."
      | otherwise -> do
          let parsedArgs = maybe [] (parseTestArgs . T.unpack) testArgs
          componentResults <-
            runTestSuite
              RunTestSuiteOptions
                { packageName = T.unpack <$> package,
                  testArguments = parsedArgs
                }
          let rendered = renderRunTestSuiteResult package parsedArgs componentResults
          pure (appendPartialLoadWarning loadResult "Test execution may be incomplete." rendered)

renderRunTestSuiteResult :: Maybe Text -> [String] -> [TestSuiteComponentResult] -> Text
renderRunTestSuiteResult packageFilter args componentResults =
  case componentResults of
    [] ->
      "No test components found"
        <> T.pack (renderPackageFilterSuffix packageFilter)
        <> "."
    _ ->
      T.pack (intercalate "\n" (headerLines <> concatMap renderComponentResult componentResults))
  where
    successCount =
      length [() | TestSuiteComponentResult {status = TestSuiteComponentExecutionSuccess _} <- componentResults]
    executionFailureCount =
      length [() | TestSuiteComponentResult {status = TestSuiteComponentExecutionFailure _} <- componentResults]
    setupFailureCount =
      length [() | TestSuiteComponentResult {status = TestSuiteComponentSetupFailure _} <- componentResults]
    headerLines =
      [ "Executed "
          <> show (length componentResults)
          <> " test components"
          <> renderPackageFilterSuffix packageFilter
          <> ".",
        "Successes: " <> show successCount <> ", execution failures: " <> show executionFailureCount <> ", setup failures: " <> show setupFailureCount <> ".",
        "Forwarded arguments: " <> show args
      ]

renderComponentResult :: TestSuiteComponentResult -> [String]
renderComponentResult TestSuiteComponentResult {packageName, componentName, moduleName, status} =
  case status of
    TestSuiteComponentSetupFailure failureReason ->
      [ "",
        "[SETUP FAIL] " <> packageName <> "/" <> componentName,
        "reason: " <> failureReason
      ]
    TestSuiteComponentExecutionFailure diagnostics ->
      [ "",
        "[FAIL] " <> packageName <> "/" <> componentName <> maybe "" (\name -> " (" <> name <> ")") moduleName
      ]
        <> case diagnostics of
          [] -> ["- no diagnostics"]
          _ -> map T.unpack (map renderDiagnosticSummary diagnostics)
    TestSuiteComponentExecutionSuccess output ->
      [ "",
        "[PASS] " <> packageName <> "/" <> componentName <> maybe "" (\name -> " (" <> name <> ")") moduleName
      ]
        <> if null output
          then ["output: <empty>"]
          else ["output:", output]

renderPackageFilterSuffix :: Maybe Text -> String
renderPackageFilterSuffix maybePackage =
  case maybePackage of
    Nothing -> ""
    Just packageName -> " for package " <> show (T.unpack packageName)

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
