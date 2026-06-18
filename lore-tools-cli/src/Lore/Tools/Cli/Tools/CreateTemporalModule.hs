module Lore.Tools.Cli.Tools.CreateTemporalModule
  ( createTemporalModuleCliTool,
  )
where

import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (noArgs)
import qualified Lore.Tools.CreateTemporalModule as CreateTemporalModule
import Lore.Tools.Render.Doc (LoreDoc)

createTemporalModuleCliTool :: CliTool LoreCliM ()
createTemporalModuleCliTool =
  CliTool
    { cliToolName = "create-temporal-module",
      cliToolAliases = ["temporal-module"],
      cliToolSummary = "Create temporal module",
      cliToolDescription = "Create and attach a temporal module to the active lore session.",
      cliToolExamples =
        [ "lore-cli create-temporal-module"
        ],
      cliToolArgs = noArgs,
      cliToolRun = successfulCliToolRun runCreateTemporalModule
    }

runCreateTemporalModule :: () -> LoreCliM LoreDoc
runCreateTemporalModule () =
  CreateTemporalModule.renderCreateTemporalModule <$> CreateTemporalModule.createTemporalModule
