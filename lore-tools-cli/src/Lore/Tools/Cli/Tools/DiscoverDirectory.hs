module Lore.Tools.Cli.Tools.DiscoverDirectory
  ( discoverDirectoryCliTool,
  )
where

import qualified Data.Text as T
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (..),
    optionalOptionWithReader,
    positionalText,
  )
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common
  ( directoryBudgetArg,
    noCompletion,
    resultLimitToMaybeInt,
  )
import qualified Lore.Tools.DiscoverDirectory as DiscoverDirectory
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result (ResultLimit)
import Options.Applicative (ReadM, eitherReader)

data DiscoverDirectoryArgs = DiscoverDirectoryArgs
  { discoverDirectoryPathArg :: FilePath,
    discoverDirectoryDepthArg :: Maybe Int,
    discoverDirectoryBudgetArg :: ResultLimit
  }

discoverDirectoryCliTool :: CliTool LoreCliM DiscoverDirectoryArgs
discoverDirectoryCliTool =
  CliTool
    { cliToolName = "discover-directory",
      cliToolAliases = [],
      cliToolSummary = "Discover directory tree",
      cliToolDescription = "Render a directory in compact or tree form with optional depth and budget.",
      cliToolExamples =
        [ "lore-cli discover-directory .",
          "lore-cli discover-directory lore/src --depth 2 --directory-budget 150"
        ],
      cliToolArgs = discoverDirectoryArgs,
      cliToolRun = successfulCliToolRun runDiscoverDirectory
    }

discoverDirectoryArgs :: CliArgs m DiscoverDirectoryArgs
discoverDirectoryArgs =
  DiscoverDirectoryArgs
    <$> (T.unpack <$> positionalText "PATH" "Path to inspect" fileOrDirectoryCompletion)
    <*> optionalOptionWithReader intReader "depth" Nothing "N" "Directory depth" noCompletion
    <*> directoryBudgetArg

runDiscoverDirectory :: DiscoverDirectoryArgs -> LoreCliM LoreDoc
runDiscoverDirectory args = do
  output <-
    DiscoverDirectory.discoverDirectory
      DiscoverDirectory.DiscoverDirectoryOptions
        { discoverDirectoryPath = args.discoverDirectoryPathArg,
          discoverDirectoryDepth = args.discoverDirectoryDepthArg,
          discoverDirectoryBudget = resultLimitToMaybeInt args.discoverDirectoryBudgetArg
        }
  pure
    ( DiscoverDirectory.renderDiscoverDirectory
        (discoverDirectoryRenderMode args.discoverDirectoryDepthArg)
        output
    )

discoverDirectoryRenderMode :: Maybe Int -> DiscoverDirectory.DiscoverDirectoryRenderMode
discoverDirectoryRenderMode = \case
  Just 0 -> DiscoverDirectory.DiscoverDirectoryRenderCompact
  _ -> DiscoverDirectory.DiscoverDirectoryRenderTree

intReader :: ReadM Int
intReader =
  eitherReader \raw ->
    case reads raw of
      [(value, "")] -> Right value
      _ -> Left "expected integer"

fileOrDirectoryCompletion :: CompletionProvider m
fileOrDirectoryCompletion = FileCompletion
