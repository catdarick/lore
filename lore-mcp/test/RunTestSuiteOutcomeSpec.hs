module RunTestSuiteOutcomeSpec
  ( spec,
  )
where

import Lore
  ( HomeModulesLoadSummary (..),
    LoadHomeModulesResult (..),
    ProjectEnvironmentFailure (..),
    TestSuiteComponentResult (..),
    TestSuiteComponentStatus (..),
  )
import Lore.Tools.Render.Doc (paragraph)
import Lore.Tools.Result (RenderedResult (..), ToolBlocked (..), ToolRun (..))
import Lore.Tools.RunTestSuite
  ( RunTestSuiteExecution (..),
    RunTestSuiteExecutionStatus (..),
    RunTestSuiteExecutionSummary (..),
    RunTestSuiteOutcome (..),
    RunTestSuiteStatus (..),
    runTestSuiteExecutionStatus,
    runTestSuiteExecutionSummary,
    runTestSuiteStatus,
  )
import Test.Hspec

spec :: Spec
spec =
  describe "RunTestSuite outcome classification" do
    it "classifies compilation failures distinctly from tests-failed" do
      let toolRun =
            ToolRunReady
              RenderedResult
                { renderedResultValue = RunTestSuiteLoadFailed (LoadHomeModulesCompleted (mkLoadSummary False)),
                  renderedResultDocument = paragraph "compile failed"
                }
      runTestSuiteStatus toolRun `shouldBe` RunTestSuiteStatusCompilationFailure

    it "classifies project environment failures" do
      runTestSuiteStatus (loadFailedToolRun (LoadHomeModulesPreparationFailed (ProjectEnvironmentFailed "bad project")))
        `shouldBe` RunTestSuiteStatusEnvironmentFailure

    it "classifies project toolchain changes as restart-required" do
      runTestSuiteStatus (loadFailedToolRun (LoadHomeModulesPreparationFailed (ProjectEnvironmentRestartRequired "restart Lore")))
        `shouldBe` RunTestSuiteStatusRestartRequired

    it "classifies invalid test arguments" do
      let toolRun =
            ToolRunReady
              RenderedResult
                { renderedResultValue = RunTestSuiteInvalidArguments "bad args",
                  renderedResultDocument = paragraph "Invalid testArgs"
                }
      runTestSuiteStatus toolRun `shouldBe` RunTestSuiteStatusInvalidArguments

    it "classifies blocked tool runs" do
      runTestSuiteStatus (ToolRunBlocked InterpreterContextNotReady)
        `shouldBe` RunTestSuiteStatusBlocked

    it "classifies no-tests separately from tests-passed" do
      let execution =
            RunTestSuiteExecution
              { runTestSuitePackageFilter = Nothing,
                runTestSuiteEffectiveArguments = [],
                runTestSuiteComponents = []
              }
          toolRun =
            ToolRunReady
              RenderedResult
                { renderedResultValue = RunTestSuiteExecuted execution,
                  renderedResultDocument = paragraph "No test components found"
                }
      runTestSuiteExecutionStatus execution `shouldBe` RunTestSuiteExecutionNoTests
      runTestSuiteStatus toolRun `shouldBe` RunTestSuiteStatusNoTests

    it "classifies all-passing executions as tests-passed" do
      let execution =
            RunTestSuiteExecution
              { runTestSuitePackageFilter = Nothing,
                runTestSuiteEffectiveArguments = ["--match", "ok"],
                runTestSuiteComponents = [componentResult (TestSuiteComponentExecutionSuccess "ok")]
              }
      runTestSuiteExecutionStatus execution `shouldBe` RunTestSuiteExecutionPassed
      runTestSuiteStatus (executedToolRun execution) `shouldBe` RunTestSuiteStatusTestsPassed

    it "classifies setup failures as tests-failed" do
      let execution =
            RunTestSuiteExecution
              { runTestSuitePackageFilter = Nothing,
                runTestSuiteEffectiveArguments = [],
                runTestSuiteComponents = [componentResult (TestSuiteComponentSetupFailure "missing entry")]
              }
      runTestSuiteExecutionStatus execution `shouldBe` RunTestSuiteExecutionFailed
      runTestSuiteStatus (executedToolRun execution) `shouldBe` RunTestSuiteStatusTestsFailed

    it "classifies execution failures as tests-failed" do
      let execution =
            RunTestSuiteExecution
              { runTestSuitePackageFilter = Nothing,
                runTestSuiteEffectiveArguments = [],
                runTestSuiteComponents = [componentResult (TestSuiteComponentExecutionFailure [])]
              }
      runTestSuiteExecutionStatus execution `shouldBe` RunTestSuiteExecutionFailed
      runTestSuiteStatus (executedToolRun execution) `shouldBe` RunTestSuiteStatusTestsFailed

    it "classifies mixed passing and failing components as tests-failed" do
      let execution =
            RunTestSuiteExecution
              { runTestSuitePackageFilter = Nothing,
                runTestSuiteEffectiveArguments = [],
                runTestSuiteComponents =
                  [ componentResult (TestSuiteComponentExecutionSuccess "ok"),
                    componentResult (TestSuiteComponentExecutionFailure [])
                  ]
              }
      runTestSuiteExecutionStatus execution `shouldBe` RunTestSuiteExecutionFailed
      runTestSuiteStatus (executedToolRun execution) `shouldBe` RunTestSuiteStatusTestsFailed

    it "summarizes component counts consistently" do
      let execution =
            RunTestSuiteExecution
              { runTestSuitePackageFilter = Nothing,
                runTestSuiteEffectiveArguments = ["--arg"],
                runTestSuiteComponents =
                  [ componentResult (TestSuiteComponentExecutionSuccess "ok"),
                    componentResult (TestSuiteComponentExecutionFailure []),
                    componentResult (TestSuiteComponentSetupFailure "setup")
                  ]
              }
          summary = runTestSuiteExecutionSummary execution
      summary.runTestSuiteTotalComponents `shouldBe` 3
      summary.runTestSuitePassedComponents `shouldBe` 1
      summary.runTestSuiteExecutionFailures `shouldBe` 1
      summary.runTestSuiteSetupFailures `shouldBe` 1

executedToolRun :: RunTestSuiteExecution -> ToolRun (RenderedResult RunTestSuiteOutcome)
executedToolRun execution =
  ToolRunReady
    RenderedResult
      { renderedResultValue = RunTestSuiteExecuted execution,
        renderedResultDocument = paragraph "executed"
      }

loadFailedToolRun :: LoadHomeModulesResult -> ToolRun (RenderedResult RunTestSuiteOutcome)
loadFailedToolRun loadResult =
  ToolRunReady
    RenderedResult
      { renderedResultValue = RunTestSuiteLoadFailed loadResult,
        renderedResultDocument = paragraph "load failed"
      }

componentResult :: TestSuiteComponentStatus -> TestSuiteComponentResult
componentResult status =
  TestSuiteComponentResult
    { packageName = "pkg",
      componentName = "component",
      moduleName = Just "Main",
      status
    }

mkLoadSummary :: Bool -> HomeModulesLoadSummary
mkLoadSummary succeeded =
  HomeModulesLoadSummary
    { homeModulesDiagnostics = [],
      homeModulesCompilationSucceeded = succeeded,
      homeModulesLoaded = if succeeded then 3 else 2,
      homeModulesFailed = if succeeded then 0 else 1,
      homeModulesAutofixed = 0,
      homeModulesAutofixedFiles = [],
      homeModulesAutofixSummaryByFile = [],
      homeModulesTotal = 3
    }
