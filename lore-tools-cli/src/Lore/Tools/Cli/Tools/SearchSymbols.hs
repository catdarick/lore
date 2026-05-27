module Lore.Tools.Cli.Tools.SearchSymbols
  ( searchSymbolsCliTool,
  )
where

import Data.Text (Text)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (DynamicCompletion),
    positionalText,
  )
import Lore.Tools.Cli.Internal.Completion (completeSymbols)
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    defaultSessionRequirements,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (limitArg, renderToolRun)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result (ResultLimit)
import qualified Lore.Tools.SearchSymbols as SearchSymbols

data SearchSymbolsArgs = SearchSymbolsArgs
  { searchSymbolsQueryArg :: Text,
    searchSymbolsLimitArg :: ResultLimit
  }

searchSymbolsCliTool :: CliTool LoreCliM SearchSymbolsArgs
searchSymbolsCliTool =
  CliTool
    { cliToolName = "search-symbols",
      cliToolAliases = ["search"],
      cliToolSummary = "Search symbols",
      cliToolDescription = "Search for symbols by exact, fuzzy, or semantic query.",
      cliToolExamples =
        [ "lore-cli search-symbols supportValues",
          "lore-cli search-symbols \"load picture from database\""
        ],
      cliToolArgs = searchSymbolsArgs,
      cliToolRun = successfulCliToolRun runSearchSymbols,
      cliToolSession = const defaultSessionRequirements
    }

searchSymbolsArgs :: CliArgs LoreCliM SearchSymbolsArgs
searchSymbolsArgs =
  SearchSymbolsArgs
    <$> positionalText "QUERY" "Search query" (DynamicCompletion completeSymbols)
    <*> limitArg

runSearchSymbols :: SearchSymbolsArgs -> LoreCliM LoreDoc
runSearchSymbols args = do
  result <-
    SearchSymbols.searchSymbols
      SearchSymbols.SearchSymbolsOptions
        { searchSymbolsQuery = args.searchSymbolsQueryArg,
          searchSymbolsSuggestionLimit = args.searchSymbolsLimitArg
        }
  pure (renderToolRun SearchSymbols.renderSearchSymbolsReady result)
