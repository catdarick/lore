module Lore.Tools.Cli.Tools.Reload
  ( reloadCliTool,
  )
where

import Lore (HomeModulesLoadSummary (..), LoadHomeModulesResult (..))
import Lore.Tools.Cli.Internal.Annotated (CliArgs)
import Lore.Tools.Cli.Internal.Tool
  ( CliInvocationResult (..),
    CliInvocationStatus (..),
    CliTool (..),
    LoreCliM,
  )
import Lore.Tools.Cli.Tools.Common (limitArg, offsetArg)
import qualified Lore.Tools.ReloadHomeModules as ReloadHomeModules
import Lore.Tools.Result (PageRequest (..), RenderedResult (..), ResultLimit)

data ReloadArgs = ReloadArgs
  { reloadOffset :: Int,
    reloadLimit :: ResultLimit
  }

reloadCliTool :: CliTool LoreCliM ReloadArgs
reloadCliTool =
  CliTool
    { cliToolName = "reload",
      cliToolAliases = [],
      cliToolSummary = "Reload home modules",
      cliToolDescription = "Reload home modules and report diagnostics.",
      cliToolExamples =
        [ "lore-cli reload",
          "lore-cli reload --offset 20 --limit 50"
        ],
      cliToolArgs = reloadArgs,
      cliToolRun = runReload
    }

reloadArgs :: CliArgs m ReloadArgs
reloadArgs =
  ReloadArgs
    <$> offsetArg
    <*> limitArg

runReload :: ReloadArgs -> LoreCliM CliInvocationResult
runReload args = do
  result <-
    ReloadHomeModules.reloadHomeModules
      ReloadHomeModules.ReloadHomeModulesOptions
        { reloadHomeModulesDiagnosticsPageRequest =
            Just (PageRequest args.reloadOffset args.reloadLimit)
        }
  let loadResult = result.renderedResultValue
      loreDoc = result.renderedResultDocument
  pure
    CliInvocationResult
      { cliInvocationResultDoc = loreDoc,
        cliInvocationResultStatus =
          case loadResult of
            LoadHomeModulesCompleted summary
              | summary.homeModulesCompilationSucceeded -> CliInvocationSucceeded
            _ -> CliInvocationFailed
      }
