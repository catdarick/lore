module Lore.Tools.Cli.Tools.ResolveInstance
  ( resolveInstanceCliTool,
  )
where

import Data.Text (Text)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    positionalText,
  )
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (noCompletion)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import qualified Lore.Tools.ResolveInstance as ResolveInstance

newtype ResolveInstanceArgs = ResolveInstanceArgs
  { resolveInstanceQueryArg :: Text
  }

resolveInstanceCliTool :: CliTool LoreCliM ResolveInstanceArgs
resolveInstanceCliTool =
  CliTool
    { cliToolName = "resolve-instance",
      cliToolAliases = [],
      cliToolSummary = "Resolve class instance",
      cliToolDescription = "Resolve the class instance selected for a class application query.",
      cliToolExamples =
        [ "lore-cli resolve-instance 'Render (Maybe Foo)'"
        ],
      cliToolArgs = resolveInstanceArgs,
      cliToolRun = successfulCliToolRun runResolveInstance
    }

resolveInstanceArgs :: CliArgs m ResolveInstanceArgs
resolveInstanceArgs =
  ResolveInstanceArgs
    <$> positionalText "QUERY" "Class application query" noCompletion

runResolveInstance :: ResolveInstanceArgs -> LoreCliM LoreDoc
runResolveInstance args = do
  result <-
    ResolveInstance.resolveInstance
      ResolveInstance.ResolveInstanceOptions
        { resolveInstanceQuery = args.resolveInstanceQueryArg
        }
  pure (toLoreDoc result)
