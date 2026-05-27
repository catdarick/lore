module Lore.Tools.Cli.Tools.FindReferences
  ( findReferencesCliTool,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (DynamicCompletion),
    optionWithReader,
    positionalText,
  )
import Lore.Tools.Cli.Internal.Completion (completeSymbols)
import Lore.Tools.Cli.Internal.Parser (verbosityReader)
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    defaultSessionRequirements,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (limitArg, offsetArg, renderToolRun, staticCompletionValues)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result (PageRequest (..), ResultLimit)
import qualified Lore.Tools.FindReferences as FindReferences

data FindReferencesArgs = FindReferencesArgs
  { findReferencesSymbolArg :: Text,
    findReferencesVerbosityArg :: Text,
    findReferencesOffsetArg :: Int,
    findReferencesLimitArg :: ResultLimit
  }

findReferencesCliTool :: CliTool LoreCliM FindReferencesArgs
findReferencesCliTool =
  CliTool
    { cliToolName = "find-references",
      cliToolAliases = ["refs", "references"],
      cliToolSummary = "Find symbol references",
      cliToolDescription = "List source references for a symbol.",
      cliToolExamples =
        [ "lore-cli find-references Demo.lookupOrZero",
          "lore-cli refs Demo.lookupOrZero --verbosity high --limit 20"
        ],
      cliToolArgs = findReferencesArgs,
      cliToolRun = successfulCliToolRun runFindReferences,
      cliToolSession = const defaultSessionRequirements
    }

findReferencesArgs :: CliArgs LoreCliM FindReferencesArgs
findReferencesArgs =
  FindReferencesArgs
    <$> positionalText "SYMBOL" "Symbol query" (DynamicCompletion completeSymbols)
    <*> optionWithReader
      verbosityReader
      "verbosity"
      Nothing
      "low|medium|high"
      "Reference rendering verbosity"
      (Just T.unpack)
      (Just "medium")
      (staticCompletionValues ["low", "medium", "high"])
    <*> offsetArg
    <*> limitArg

runFindReferences :: FindReferencesArgs -> LoreCliM LoreDoc
runFindReferences args = do
  result <-
    FindReferences.findReferences
      FindReferences.FindReferencesOptions
        { findReferencesQuery = args.findReferencesSymbolArg,
          findReferencesPageRequest =
            PageRequest args.findReferencesOffsetArg args.findReferencesLimitArg,
          findReferencesVerbosity = parseVerbosity args.findReferencesVerbosityArg
        }
  pure (renderToolRun FindReferences.renderFindReferencesOutput result)

parseVerbosity :: Text -> FindReferences.FindReferencesVerbosity
parseVerbosity rawVerbosity =
  case T.toLower rawVerbosity of
    "low" -> FindReferences.Low
    "high" -> FindReferences.High
    _ -> FindReferences.Medium
