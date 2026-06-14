module Lore.Mcp.Tools.RunTestSuite
  ( runTestSuiteTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (HomeModulesLoadSummary (..), LoadHomeModulesResult (..), MonadLore, projectEnvironmentFailureMessage)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Tools.Result (RenderedResult (..), ToolRun (..))
import Lore.Tools.RunTestSuite
  ( RunTestSuiteExecution (..),
    RunTestSuiteOutcome (..),
    RunTestSuiteStatus (..),
    RunTestSuiteToolOptions (..),
    runTestSuiteStatus,
  )
import qualified Lore.Tools.RunTestSuite as ToolsRunTestSuite

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
  SomeToolWithArgsStructured
    ToolWithArgs
      { name = "runTestSuite",
        description = Just "Runs the test suite. Equivalent to invoking 'cabal test' or 'stack test' in the terminal.",
        handler = runTestSuiteHandler
      }
    runTestSuiteStructured

runTestSuiteHandler :: (MonadLore m) => RunTestSuiteArgs 'ValueType -> m (ToolRun (RenderedResult RunTestSuiteOutcome))
runTestSuiteHandler RunTestSuiteArgs {package, testArgs} =
  ToolsRunTestSuite.runTestSuite
    RunTestSuiteToolOptions
      { runTestSuitePackageFilter = package,
        runTestSuiteRawArgs = testArgs
      }

runTestSuiteStructured :: RunTestSuiteArgs 'ValueType -> ToolRun (RenderedResult RunTestSuiteOutcome) -> J.Value
runTestSuiteStructured args result =
  case result of
    ToolRunBlocked _ ->
      J.object
        [ "tool" J..= ("runTestSuite" :: String),
          "status" J..= statusText,
          "invocation" J..= requestedInvocation args
        ]
    ToolRunReady renderedResult ->
      case renderedResult.renderedResultValue of
        RunTestSuiteLoadFailed loadResult -> loadFailureObject args statusText loadResult
        RunTestSuiteInvalidArguments parseErrorMessage ->
          J.object
            [ "tool" J..= ("runTestSuite" :: String),
              "status" J..= statusText,
              "invocation" J..= requestedInvocation args,
              "message" J..= parseErrorMessage
            ]
        RunTestSuiteExecuted execution ->
          J.object
            [ "tool" J..= ("runTestSuite" :: String),
              "status" J..= statusText,
              "invocation" J..= executedInvocation execution
            ]
  where
    statusText = runTestSuiteStatusText (runTestSuiteStatus result)

requestedInvocation :: RunTestSuiteArgs 'ValueType -> J.Value
requestedInvocation RunTestSuiteArgs {package, testArgs} =
  J.object
    [ "package" J..= package,
      "requestedTestArgs" J..= testArgs
    ]

executedInvocation :: RunTestSuiteExecution -> J.Value
executedInvocation execution =
  J.object
    [ "package" J..= execution.runTestSuitePackageFilter,
      "effectiveArguments" J..= execution.runTestSuiteEffectiveArguments
    ]

runTestSuiteStatusText :: RunTestSuiteStatus -> String
runTestSuiteStatusText = \case
  RunTestSuiteStatusCompilationFailure -> "compilation-failure"
  RunTestSuiteStatusEnvironmentFailure -> "environment-failure"
  RunTestSuiteStatusRestartRequired -> "restart-required"
  RunTestSuiteStatusInvalidArguments -> "invalid-arguments"
  RunTestSuiteStatusNoTests -> "no-tests"
  RunTestSuiteStatusTestsPassed -> "tests-passed"
  RunTestSuiteStatusTestsFailed -> "tests-failed"
  RunTestSuiteStatusBlocked -> "blocked"

loadFailureObject :: RunTestSuiteArgs 'ValueType -> String -> LoadHomeModulesResult -> J.Value
loadFailureObject args statusText = \case
  LoadHomeModulesCompleted summary ->
    J.object
      [ "tool" J..= ("runTestSuite" :: String),
        "status" J..= statusText,
        "invocation" J..= requestedInvocation args,
        "compilation"
          J..= J.object
            [ "loadedModules" J..= summary.homeModulesLoaded,
              "failedModules" J..= summary.homeModulesFailed,
              "totalModules" J..= summary.homeModulesTotal
            ]
      ]
  LoadHomeModulesPreparationFailed failure ->
    J.object
      [ "tool" J..= ("runTestSuite" :: String),
        "status" J..= statusText,
        "invocation" J..= requestedInvocation args,
        "message" J..= projectEnvironmentFailureMessage failure
      ]
