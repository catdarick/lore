module Lore.Tools.Cli.Tools.RunTestSuite
  ( runTestSuiteCliTool,
  )
where

import Data.Text (Text)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (DynamicCompletion),
    optionalOptionText,
  )
import Lore.Tools.Cli.Internal.Completion (completePackages)
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (noCompletion)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import qualified Lore.Tools.RunTestSuite as RunTestSuite

data RunTestSuiteArgs = RunTestSuiteArgs
  { runTestSuitePackageArg :: Maybe Text,
    runTestSuiteRawArgsArg :: Maybe Text
  }

runTestSuiteCliTool :: CliTool LoreCliM RunTestSuiteArgs
runTestSuiteCliTool =
  CliTool
    { cliToolName = "run-test-suite",
      cliToolAliases = [],
      cliToolSummary = "Run project tests",
      cliToolDescription = "Run package tests through lore-tools test-suite integration.",
      cliToolExamples =
        [ "lore-cli run-test-suite",
          "lore-cli run-test-suite --package lore --test-args '--match \"some test\"'"
        ],
      cliToolArgs = runTestSuiteArgs,
      cliToolRun = successfulCliToolRun runRunTestSuite
    }

runTestSuiteArgs :: CliArgs LoreCliM RunTestSuiteArgs
runTestSuiteArgs =
  RunTestSuiteArgs
    <$> optionalOptionText "package" Nothing "PKG" "Optional package filter" (DynamicCompletion completePackages)
    <*> optionalOptionText "test-args" Nothing "ARGS" "Raw args forwarded to test runner" noCompletion

runRunTestSuite :: RunTestSuiteArgs -> LoreCliM LoreDoc
runRunTestSuite args = do
  result <-
    RunTestSuite.runTestSuite
      RunTestSuite.RunTestSuiteToolOptions
        { runTestSuitePackageFilter = args.runTestSuitePackageArg,
          runTestSuiteRawArgs = args.runTestSuiteRawArgsArg
        }
  pure (toLoreDoc result)
