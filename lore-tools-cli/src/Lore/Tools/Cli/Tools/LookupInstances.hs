module Lore.Tools.Cli.Tools.LookupInstances
  ( lookupInstancesCliTool,
  )
where

import Data.Text (Text)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (DynamicCompletion),
    somePositionalText,
  )
import Lore.Tools.Cli.Internal.Completion (completeSymbols)
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (limitArg, offsetArg)
import qualified Lore.Tools.LookupInstances as LookupInstances
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.Result (PageRequest (..), ResultLimit)

data LookupInstancesArgs = LookupInstancesArgs
  { lookupInstancesNamesArg :: [Text],
    lookupInstancesOffsetArg :: Int,
    lookupInstancesLimitArg :: ResultLimit
  }

lookupInstancesCliTool :: CliTool LoreCliM LookupInstancesArgs
lookupInstancesCliTool =
  CliTool
    { cliToolName = "lookup-instances",
      cliToolAliases = [],
      cliToolSummary = "Lookup class/family instances",
      cliToolDescription = "Find indexed class or family instances that mention all queried names.",
      cliToolExamples =
        [ "lore-cli lookup-instances Show Int",
          "lore-cli lookup-instances Render Maybe Foo --limit 10"
        ],
      cliToolArgs = lookupInstancesArgs,
      cliToolRun = successfulCliToolRun runLookupInstances
    }

lookupInstancesArgs :: CliArgs LoreCliM LookupInstancesArgs
lookupInstancesArgs =
  LookupInstancesArgs
    <$> somePositionalText "NAME" "Class/type/symbol name" (DynamicCompletion completeSymbols)
    <*> offsetArg
    <*> limitArg

runLookupInstances :: LookupInstancesArgs -> LoreCliM LoreDoc
runLookupInstances args = do
  result <-
    LookupInstances.lookupInstances
      LookupInstances.LookupInstancesOptions
        { lookupInstancesNames = args.lookupInstancesNamesArg,
          lookupInstancesPageRequest =
            PageRequest args.lookupInstancesOffsetArg args.lookupInstancesLimitArg
        }
  pure (toLoreDoc result)
