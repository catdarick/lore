module Lore.Tools.Cli.Tools.SearchSymbols
  ( searchSymbolsCliTool,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (DynamicCompletion),
    manyOptionWithReader,
    positionalText,
  )
import Lore.Tools.Cli.Internal.Completion (completeSymbols)
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    defaultSessionRequirements,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (limitArg, noCompletion, renderToolRun)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result (ResultLimit)
import qualified Lore.Tools.SearchSymbols as SearchSymbols
import Options.Applicative (ReadM, eitherReader)

data SearchSymbolsArgs = SearchSymbolsArgs
  { searchSymbolsQueryArg :: Text,
    searchSymbolsLimitArg :: ResultLimit,
    searchSymbolsModulePatternArgs :: [SearchSymbols.SearchSymbolsModulePattern]
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
          "lore-cli search-symbols \"load picture from database\"",
          "lore-cli search-symbols createUser --module-pattern \"Placid.Gateways.*\" --module-pattern \"ExternalProviders.*.Database.*\""
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
    <*> modulePatternArg

modulePatternArg :: CliArgs m [SearchSymbols.SearchSymbolsModulePattern]
modulePatternArg =
  manyOptionWithReader
    modulePatternReader
    "module-pattern"
    Nothing
    "PATTERN"
    "Only include symbols associated with modules matching at least one PATTERN. May be passed multiple times. '*' matches any sequence of characters. Matching is case-sensitive and covers the complete module name."
    noCompletion

modulePatternReader :: ReadM SearchSymbols.SearchSymbolsModulePattern
modulePatternReader =
  eitherReader \raw ->
    case SearchSymbols.mkSearchSymbolsModulePattern (T.pack raw) of
      Right modulePattern -> Right modulePattern
      Left _ -> Left "module pattern must not be empty"

runSearchSymbols :: SearchSymbolsArgs -> LoreCliM LoreDoc
runSearchSymbols args = do
  result <-
    SearchSymbols.searchSymbols
      SearchSymbols.SearchSymbolsOptions
        { searchSymbolsQuery = args.searchSymbolsQueryArg,
          searchSymbolsSuggestionLimit = args.searchSymbolsLimitArg,
          searchSymbolsModulePatterns = args.searchSymbolsModulePatternArgs
        }
  pure (renderToolRun SearchSymbols.renderSearchSymbolsOutput result)
