module Lore.Tools.Cli.Tools.DebugCacheMemory
  ( debugCacheMemoryCliTool,
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
import qualified Lore.Tools.DebugCacheMemory as DebugCacheMemory

debugCacheMemoryCliTool :: CliTool LoreCliM ()
debugCacheMemoryCliTool =
  CliTool
    { cliToolName = "debug-cache-memory",
      cliToolAliases = ["cache-memory-debug"],
      cliToolSummary = "Measure memory impact of clearing each cache",
      cliToolDescription = "Clear each cache one-by-one, force evaluation, run repeated major GCs, and report RTS memory deltas.",
      cliToolExamples =
        [ "lore-cli debug-cache-memory"
        ],
      cliToolArgs = noArgs,
      cliToolRun = successfulCliToolRun runDebugCacheMemory,
      cliToolSession = const defaultSessionRequirements
    }

runDebugCacheMemory :: () -> LoreCliM LoreDoc
runDebugCacheMemory () =
  DebugCacheMemory.renderDebugCacheMemory <$> DebugCacheMemory.debugCacheMemory
