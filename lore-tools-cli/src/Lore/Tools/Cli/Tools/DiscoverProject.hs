module Lore.Tools.Cli.Tools.DiscoverProject
  ( discoverProjectCliTool,
  )
where

import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    defaultSessionRequirements,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (noArgs)
import Lore.Tools.Render.Doc (LoreDoc)
import qualified Lore.Tools.DiscoverProject as DiscoverProject

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
      cliToolRun = successfulCliToolRun runDiscoverProject,
      cliToolSession = const defaultSessionRequirements
    }

runDiscoverProject :: () -> LoreCliM LoreDoc
runDiscoverProject () =
  DiscoverProject.renderDiscoverProject <$> DiscoverProject.discoverProject
