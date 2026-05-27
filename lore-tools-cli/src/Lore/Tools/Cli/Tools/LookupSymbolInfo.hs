module Lore.Tools.Cli.Tools.LookupSymbolInfo
  ( lookupSymbolInfoCliTool,
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
import Lore.Tools.Cli.Tools.Common (limitArg, offsetArg, renderToolRun)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result (PageRequest (..), ResultLimit)
import qualified Lore.Tools.LookupSymbolInfo as LookupSymbolInfo

data LookupSymbolInfoArgs = LookupSymbolInfoArgs
  { lookupSymbolInfoSymbolArg :: Text,
    lookupSymbolInfoOffsetArg :: Int,
    lookupSymbolInfoLimitArg :: ResultLimit
  }

lookupSymbolInfoCliTool :: CliTool LoreCliM LookupSymbolInfoArgs
lookupSymbolInfoCliTool =
  CliTool
    { cliToolName = "lookup-symbol-info",
      cliToolAliases = ["info"],
      cliToolSummary = "Lookup symbol metadata",
      cliToolDescription = "Show metadata, signatures, and definition locations for symbols.",
      cliToolExamples =
        [ "lore-cli lookup-symbol-info Demo.lookupOrZero",
          "lore-cli info lookupOrZero --limit 20"
        ],
      cliToolArgs = lookupSymbolInfoArgs,
      cliToolRun = successfulCliToolRun runLookupSymbolInfo,
      cliToolSession = const defaultSessionRequirements
    }

lookupSymbolInfoArgs :: CliArgs LoreCliM LookupSymbolInfoArgs
lookupSymbolInfoArgs =
  LookupSymbolInfoArgs
    <$> positionalText "SYMBOL" "Symbol query" (DynamicCompletion completeSymbols)
    <*> offsetArg
    <*> limitArg

runLookupSymbolInfo :: LookupSymbolInfoArgs -> LoreCliM LoreDoc
runLookupSymbolInfo args = do
  result <-
    LookupSymbolInfo.lookupSymbolInfo
      LookupSymbolInfo.LookupSymbolInfoOptions
        { lookupSymbolInfoQuery = args.lookupSymbolInfoSymbolArg,
          lookupSymbolInfoPageRequest =
            PageRequest args.lookupSymbolInfoOffsetArg args.lookupSymbolInfoLimitArg,
          lookupSymbolInfoSuggestionLimit = args.lookupSymbolInfoLimitArg
        }
  pure (renderToolRun LookupSymbolInfo.renderLookupSymbolInfoReady result)
