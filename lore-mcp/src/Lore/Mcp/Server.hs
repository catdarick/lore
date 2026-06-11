module Lore.Mcp.Server
  ( runLoreMcpServer,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as T
import Lore
  ( SessionConfig (..),
    loadStartupConfig,
    renderSessionConfigError,
    startupConfigDocument,
    startupSessionConfig,
  )
import Lore.Mcp.Config
  ( McpConfig (..),
    defaultMcpConfig,
    loadMcpEnvironmentOverrides,
    parseMcpYamlConfig,
    renderMcpConfigError,
    resolveMcpConfig,
    toolEnabled,
  )
import Lore.Mcp.Internal.Tool (SomeTool, getToolName)
import Lore.Mcp.Monad (LoreMcpMonad, newLoreMcpContext, runLoreMcp)
import Lore.Mcp.Protocol.Server (McpServer (..), runMcpServer)
import Lore.Mcp.Tools.CreateTemporalModule (createTemporalModuleTool)
import Lore.Mcp.Tools.DiscoverDirectory (discoverDirectoryTool)
import Lore.Mcp.Tools.DiscoverProject (discoverProjectTool)
import Lore.Mcp.Tools.ExecuteCode (executeCodeTool)
import Lore.Mcp.Tools.Feedback (feedbackTool)
import Lore.Mcp.Tools.FindDeadCode (findDeadCodeTool)
import Lore.Mcp.Tools.FindReferences (findReferencesTool)
import Lore.Mcp.Tools.GetDefinition.Cached (cachedGetDefinitionTool)
import Lore.Mcp.Tools.GetDefinition.Regular (regularGetDefinitionTool)
import Lore.Mcp.Tools.GetTypeOfExpression (getTypeOfExpressionTool)
import Lore.Mcp.Tools.ListExportedSymbols (listExportedSymbolsTool)
import Lore.Mcp.Tools.LookupInstances (lookupInstancesTool)
import Lore.Mcp.Tools.LookupSymbolInfo (lookupSymbolInfoTool)
import Lore.Mcp.Tools.NotifyKnowledgeReset (notifyKnowledgeResetTool)
import Lore.Mcp.Tools.ReloadHomeModules (reloadHomeModulesTool)
import Lore.Mcp.Tools.ResolveInstance (resolveInstanceTool)
import Lore.Mcp.Tools.RunTestSuite (runTestSuiteTool)
import Lore.Mcp.Tools.SearchSymbols (searchSymbolsTool)
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)

runLoreMcpServer :: IO ()
runLoreMcpServer = do
  startupConfig <-
    loadStartupConfig >>= either failWithSessionConfigError pure
  let knownToolNames =
        Set.fromList
          ( map
              getToolName
              (getTools True True (Just "__known_tool_names__") :: [SomeTool LoreMcpMonad])
          )
  yamlMcpOverrides <-
    either failWithMcpConfigError pure (parseMcpYamlConfig knownToolNames startupConfig.startupConfigDocument)
  environmentMcpOverrides <-
    loadMcpEnvironmentOverrides knownToolNames >>= either failWithMcpConfigError pure
  let mcpConfig =
        resolveMcpConfig defaultMcpConfig yamlMcpOverrides environmentMcpOverrides
      runTestSuiteToolEnabled =
        toolEnabled mcpConfig "runTestSuite"
      definitionKnowledgeCacheEnabled =
        mcpConfig.definitionKnowledgeCacheEnabled
      notifyKnowledgeResetToolEnabled =
        toolEnabled mcpConfig "notifyKnowledgeReset"
  let sessionConfig =
        startupConfig.startupSessionConfig
          { isTestSuiteFunctionalityRequired = runTestSuiteToolEnabled
          }
  mcpContext <- newLoreMcpContext definitionKnowledgeCacheEnabled
  let tools =
        getTools
          definitionKnowledgeCacheEnabled
          notifyKnowledgeResetToolEnabled
          mcpConfig.feedbackFilePath
      enabledTools =
        filterEnabledTools mcpConfig tools
  runLoreMcp sessionConfig mcpContext do
    runMcpServer
      McpServer
        { name = "lore",
          initialize = pure (),
          tools = enabledTools,
          renderer = renderLoreDocMarkdown
        }
  where
    getTools definitionKnowledgeCacheEnabled notifyKnowledgeResetToolEnabled maybeFeedbackFilePath =
      let feedbackTools =
            case maybeFeedbackFilePath of
              Just feedbackFilePath
                | not (null feedbackFilePath) ->
                    [feedbackTool feedbackFilePath]
              _ ->
                []
          definitionKnowledgeTools =
            if definitionKnowledgeCacheEnabled
              then [notifyKnowledgeResetTool]
              else []
          getDefinitionTool =
            if definitionKnowledgeCacheEnabled
              then cachedGetDefinitionTool notifyKnowledgeResetToolEnabled
              else regularGetDefinitionTool
       in [ reloadHomeModulesTool,
            discoverProjectTool,
            discoverDirectoryTool,
            listExportedSymbolsTool,
            searchSymbolsTool,
            lookupSymbolInfoTool,
            getDefinitionTool,
            findDeadCodeTool,
            resolveInstanceTool,
            findReferencesTool,
            lookupInstancesTool,
            createTemporalModuleTool,
            getTypeOfExpressionTool,
            executeCodeTool,
            runTestSuiteTool
          ]
            <> definitionKnowledgeTools
            <> feedbackTools

    filterEnabledTools :: McpConfig -> [SomeTool m] -> [SomeTool m]
    filterEnabledTools config =
      filter \tool ->
        toolEnabled config (getToolName tool)

    failWithSessionConfigError =
      ioError . userError . T.unpack . renderSessionConfigError

    failWithMcpConfigError =
      ioError . userError . T.unpack . renderMcpConfigError
