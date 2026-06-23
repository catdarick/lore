module Lore.Tools.Cli.Tools.RtsStats
  ( rtsStatsCliTool,
  )
where

import qualified Lore.Tools.RtsStats as RtsStats
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (noArgs)
import Lore.Tools.Render.Doc (LoreDoc)

rtsStatsCliTool :: CliTool LoreCliM ()
rtsStatsCliTool =
  CliTool
    { cliToolName = "rts-stats",
      cliToolAliases = ["rts"],
      cliToolSummary = "Print RTS stats",
      cliToolDescription = "Print GHC RTS statistics for the lore-cli process.",
      cliToolExamples =
        [ "lore-cli rts-stats",
          "lore-cli rts"
        ],
      cliToolArgs = noArgs,
      cliToolRun = successfulCliToolRun runRtsStats
    }

runRtsStats :: () -> LoreCliM LoreDoc
runRtsStats () =
  RtsStats.renderRtsStats <$> RtsStats.rtsStats
