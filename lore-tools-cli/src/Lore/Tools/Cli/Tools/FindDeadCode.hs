module Lore.Tools.Cli.Tools.FindDeadCode
  ( findDeadCodeCliTool,
  )
where

import Data.Text (Text)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (DynamicCompletion),
    manyOptionText,
  )
import Lore.Tools.Cli.Internal.Completion (completeLoadedModules)
import Lore.Tools.Cli.Internal.Tool
  ( CliInvocationResult (..),
    CliInvocationStatus (..),
    CliTool (..),
    LoreCliM,
  )
import Lore.Tools.Cli.Tools.Common (limitArg, offsetArg, renderToolRun)
import qualified Lore.Tools.FindDeadCode as FindDeadCode
import Lore.Tools.Result (PageRequest (..), ResultLimit, ToolRun (..))

data FindDeadCodeArgs = FindDeadCodeArgs
  { findDeadCodeModulesArg :: [Text],
    findDeadCodeOffsetArg :: Int,
    findDeadCodeLimitArg :: ResultLimit
  }

findDeadCodeCliTool :: CliTool LoreCliM FindDeadCodeArgs
findDeadCodeCliTool =
  CliTool
    { cliToolName = "find-dead-code",
      cliToolAliases = ["dead-code"],
      cliToolSummary = "Find dead declarations",
      cliToolDescription = "Report top-level declarations that are not reachable from alive roots.",
      cliToolExamples =
        [ "lore-cli find-dead-code",
          "lore-cli find-dead-code --module Demo --limit 20"
        ],
      cliToolArgs = findDeadCodeArgs,
      cliToolRun = runFindDeadCode
    }

findDeadCodeArgs :: CliArgs LoreCliM FindDeadCodeArgs
findDeadCodeArgs =
  FindDeadCodeArgs
    <$> manyOptionText "module" Nothing "MODULE" "Restrict to module" (DynamicCompletion completeLoadedModules)
    <*> offsetArg
    <*> limitArg

runFindDeadCode :: FindDeadCodeArgs -> LoreCliM CliInvocationResult
runFindDeadCode args = do
  result <-
    FindDeadCode.findDeadCode
      FindDeadCode.FindDeadCodeOptions
        { findDeadCodeModules =
            if null args.findDeadCodeModulesArg then Nothing else Just args.findDeadCodeModulesArg,
          findDeadCodePageRequest =
            Just (PageRequest args.findDeadCodeOffsetArg args.findDeadCodeLimitArg)
        }
  pure
    CliInvocationResult
      { cliInvocationResultDoc = renderToolRun FindDeadCode.renderFindDeadCodeOutput result,
        cliInvocationResultStatus = deadCodeStatus result
      }

deadCodeStatus :: FindDeadCode.FindDeadCodeResult -> CliInvocationStatus
deadCodeStatus = \case
  ToolRunBlocked _ ->
    CliInvocationFailed
  ToolRunReady output ->
    case output of
      FindDeadCode.FindDeadCodeFailed _ ->
        CliInvocationFailed
      FindDeadCode.FindDeadCodeReadyResult ready
        | ready.findDeadCodeHasDeadDefinitions ->
            CliInvocationFailed
        | otherwise ->
            CliInvocationSucceeded
