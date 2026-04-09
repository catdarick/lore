module Lore.Mcp.Server
  ( runLoreMcpServer,
  )
where

import Lore (ParallelWorkersCount (..), PreludeImportRule (..), SessionConfig (..), noLogHandle, runLore)
import Lore.Mcp.Protocol.Server (McpServer (..), runMcpServer)
import Lore.Mcp.Tools.ExecuteStatement (executeStatementTool)
import Lore.Mcp.Tools.GetDefinition (getDefinitionTool)
import Lore.Mcp.Tools.GetTypeOfExpression (getTypeOfExpressionTool)
import Lore.Mcp.Tools.LookupInstances (lookupInstancesTool)
import Lore.Mcp.Tools.LookupSymbolInfo (lookupSymbolInfoTool)
import Lore.Mcp.Tools.ReloadHomeModules (reloadHomeModulesTool)

runLoreMcpServer :: IO ()
runLoreMcpServer = runLore sessionConfig do
  runMcpServer
    McpServer
      { name = "lore",
        initialize = pure (),
        tools = [reloadHomeModulesTool, executeStatementTool, getTypeOfExpressionTool, lookupSymbolInfoTool, lookupInstancesTool, getDefinitionTool]
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
