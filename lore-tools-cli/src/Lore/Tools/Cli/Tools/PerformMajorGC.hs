module Lore.Tools.Cli.Tools.PerformMajorGC
  ( performMajorGCCliTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (noArgs)
import Lore.Tools.Render.Doc
  ( LoreDoc,
    paragraph,
  )
import System.Mem (performMajorGC)

performMajorGCCliTool :: CliTool LoreCliM ()
performMajorGCCliTool =
  CliTool
    { cliToolName = "perform-major-gc",
      cliToolAliases = ["major-gc"],
      cliToolSummary = "Run a major GC",
      cliToolDescription = "Run System.Mem.performMajorGC in the lore-cli process.",
      cliToolExamples =
        [ "lore-cli perform-major-gc",
          "lore-cli major-gc"
        ],
      cliToolArgs = noArgs,
      cliToolRun = successfulCliToolRun runPerformMajorGC
    }

runPerformMajorGC :: () -> LoreCliM LoreDoc
runPerformMajorGC () = do
  liftIO performMajorGC
  pure (paragraph "Major GC completed.")
