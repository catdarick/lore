module Lore.Tools.Cli.Tools.DiscoverProject
  ( discoverProjectCliTool,
  )
where

import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (noArgs)
import qualified Lore.Tools.DiscoverProject as DiscoverProject
import Lore.Tools.Render.Doc (LoreDoc)

discoverProjectCliTool :: CliTool LoreCliM ()
discoverProjectCliTool =
  CliTool
    { cliToolName = "discover-project",
      cliToolAliases = [],
      cliToolSummary = "Discover project packages",
      cliToolDescription = "Scan the workspace for package.yaml files and render package structure.",
      cliToolExamples =
        [ "lore-cli discover-project"
        ],
      cliToolArgs = noArgs,
      cliToolRun = successfulCliToolRun runDiscoverProject
    }

runDiscoverProject :: () -> LoreCliM LoreDoc
runDiscoverProject () =
  DiscoverProject.renderDiscoverProject <$> DiscoverProject.discoverProject
