module Lore.Mcp.Server
  ( runLoreMcpServer,
  )
where

import Lore (ParallelWorkersCount (..), PreludeImportRule (..), SessionConfig (..), noLogHandle, runLore)
import Lore.Mcp.Protocol.Server (McpServer (..), runMcpServer)
import Lore.Mcp.Tools.LoadTargets (loadTargetsTool)

runLoreMcpServer :: IO ()
runLoreMcpServer = runLore sessionConfig do
  runMcpServer
    McpServer
      { name = "lore-mcp",
        initialize = pure (),
        tools = [loadTargetsTool]
      }
  where
    sessionConfig =
      SessionConfig
        { projectRoot = ".",
          ghcWorkDir = ".lore-work",
          loggerHandle = noLogHandle,
          interpreterPreludeImportRule = ImportBasePrelude,
          parallelWorkersLimit = WorkersAsNumProcessors
        }
