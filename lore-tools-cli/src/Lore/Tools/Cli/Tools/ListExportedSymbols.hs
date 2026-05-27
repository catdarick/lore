module Lore.Tools.Cli.Tools.ListExportedSymbols
  ( listExportedSymbolsCliTool,
  )
where

import Data.Text (Text)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (DynamicCompletion),
    optionalOptionText,
    positionalText,
  )
import Lore.Tools.Cli.Internal.Completion (completeLoadedModules, completePackages)
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    defaultSessionRequirements,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (limitArg, noCompletion, offsetArg, renderToolRun)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result (PageRequest (..), ResultLimit)
import qualified Lore.Tools.ListExportedSymbols as ListExportedSymbols

data ListExportedSymbolsArgs = ListExportedSymbolsArgs
  { listExportedModuleArg :: Text,
    listExportedPackageArg :: Maybe Text,
    listExportedTypeHintArg :: Maybe Text,
    listExportedOffsetArg :: Int,
    listExportedLimitArg :: ResultLimit
  }

listExportedSymbolsCliTool :: CliTool LoreCliM ListExportedSymbolsArgs
listExportedSymbolsCliTool =
  CliTool
    { cliToolName = "list-exported-symbols",
      cliToolAliases = [],
      cliToolSummary = "List module exports",
      cliToolDescription = "List exported symbols for a module, with optional package/type filters.",
      cliToolExamples =
        [ "lore-cli list-exported-symbols Demo",
          "lore-cli list-exported-symbols Data.Map --package containers --type-hint Text"
        ],
      cliToolArgs = listExportedSymbolsArgs,
      cliToolRun = successfulCliToolRun runListExportedSymbols,
      cliToolSession = const defaultSessionRequirements
    }

listExportedSymbolsArgs :: CliArgs LoreCliM ListExportedSymbolsArgs
listExportedSymbolsArgs =
  ListExportedSymbolsArgs
    <$> positionalText "MODULE" "Module name" (DynamicCompletion completeLoadedModules)
    <*> optionalOptionText "package" Nothing "PKG" "Package qualifier" (DynamicCompletion completePackages)
    <*> optionalOptionText "type-hint" Nothing "TYPE" "Optional type hint filter" noCompletion
    <*> offsetArg
    <*> limitArg

runListExportedSymbols :: ListExportedSymbolsArgs -> LoreCliM LoreDoc
runListExportedSymbols args = do
  result <-
    ListExportedSymbols.listExportedSymbols
      ListExportedSymbols.ListExportedSymbolsOptions
        { listExportedSymbolsModuleName = args.listExportedModuleArg,
          listExportedSymbolsPackageName = args.listExportedPackageArg,
          listExportedSymbolsTypeHint = args.listExportedTypeHintArg,
          listExportedSymbolsPageRequest =
            PageRequest args.listExportedOffsetArg args.listExportedLimitArg
        }
  pure (renderToolRun ListExportedSymbols.renderListExportedSymbolsReady result)
