module Lore.Mcp.Server
  ( runLoreMcpServer,
  )
where

import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isNothing)
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
  ( CustomCommandToolConfig (..),
    McpConfig (..),
    McpConfigOverrides (..),
    defaultMcpConfig,
    loadMcpEnvironmentOverrides,
    parseMcpYamlConfig,
    renderMcpConfigError,
    resolveMcpConfig,
    toolEnabled,
  )
import Lore.Mcp.Internal.Tool (SomeTool, getToolName)
import Lore.Mcp.KnowledgeCacheRpc (knowledgeCacheRequestHandlers)
import Lore.Mcp.Monad (LoreMcpMonad, newLoreMcpContext, runLoreMcp)
import Lore.Mcp.Protocol.Server (McpServer (..), runMcpServer)
import Lore.Mcp.StructuredToolRpc (structuredToolRequestHandlers)
import Lore.Mcp.Tools.CreateTemporalModule (createTemporalModuleTool)
import Lore.Mcp.Tools.CustomCommand (customCommandTool)
import Lore.Mcp.Tools.DiscoverDirectory (discoverDirectoryTool)
import Lore.Mcp.Tools.DiscoverProject (discoverProjectTool)
import Lore.Mcp.Tools.ExecuteCode (executeCodeTool)
import Lore.Mcp.Tools.Feedback (feedbackTool)
import Lore.Mcp.Tools.FindDeadCode (findDeadCodeTool)
import Lore.Mcp.Tools.FindReferences (findReferencesTool)
import Lore.Mcp.Tools.GetDefinitions.Cached (cachedGetDefinitionsTool)
import Lore.Mcp.Tools.GetDefinitions.Regular (regularGetDefinitionTool)
import Lore.Mcp.Tools.GetTypeOfExpression (getTypeOfExpressionTool)
import Lore.Mcp.Tools.ListExportedSymbols (listExportedSymbolsTool)
import Lore.Mcp.Tools.LookupInstances (lookupInstancesTool)
import Lore.Mcp.Tools.LookupSymbolInfo (lookupSymbolInfoTool)
import Lore.Mcp.Tools.NotifyKnowledgeReset (notifyKnowledgeResetTool)
import Lore.Mcp.Tools.ReloadHomeModules (reloadHomeModulesTool)
import Lore.Mcp.Tools.ResolveInstance (resolveInstanceTool)
import Lore.Mcp.Tools.RunTestSuite (customRunTestSuiteTool, runTestSuiteTool)
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
              (getTools True True (Just "__known_tool_names__") [] :: [SomeTool LoreMcpMonad])
          )
  yamlMcpOverrides <-
    either failWithMcpConfigError pure (parseMcpYamlConfig knownToolNames startupConfig.startupConfigDocument)
  let allKnownToolNames =
        knownToolNames <> Set.fromList (map (.name) yamlMcpOverrides.customCommandToolsOverride)
  environmentMcpOverrides <-
    loadMcpEnvironmentOverrides allKnownToolNames >>= either failWithMcpConfigError pure
  let mcpConfig =
        resolveMcpConfig defaultMcpConfig yamlMcpOverrides environmentMcpOverrides
      runTestSuiteToolEnabled =
        toolEnabled mcpConfig "runTestSuite"
      customRunTestSuiteConfig =
        findRunTestSuiteOverride mcpConfig.customCommandTools
      builtInRunTestSuiteEnabled =
        runTestSuiteToolEnabled && isNothing customRunTestSuiteConfig
      definitionKnowledgeCacheEnabled =
        mcpConfig.definitionKnowledgeCacheEnabled
      notifyKnowledgeResetToolEnabled =
        toolEnabled mcpConfig "notifyKnowledgeReset"
  let sessionConfig =
        startupConfig.startupSessionConfig
          { isTestSuiteFunctionalityRequired = builtInRunTestSuiteEnabled
          }
  mcpContext <- newLoreMcpContext definitionKnowledgeCacheEnabled
  let tools =
        getTools
          definitionKnowledgeCacheEnabled
          notifyKnowledgeResetToolEnabled
          mcpConfig.feedbackFilePath
          mcpConfig.customCommandTools
      enabledTools =
        filterEnabledTools mcpConfig tools
      customRequestHandlers =
        Map.unions
          ( catMaybes
              [ Just (structuredToolRequestHandlers enabledTools renderLoreDocMarkdown),
                if definitionKnowledgeCacheEnabled
                  then Just knowledgeCacheRequestHandlers
                  else Nothing
              ]
          )
  runLoreMcp sessionConfig mcpContext do
    runMcpServer
      McpServer
        { name = "lore",
          initialize = pure (),
          tools = enabledTools,
          customRequestHandlers,
          renderer = renderLoreDocMarkdown
        }
  where
    getTools definitionKnowledgeCacheEnabled notifyKnowledgeResetToolEnabled maybeFeedbackFilePath customCommandToolConfigs =
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
              then cachedGetDefinitionsTool notifyKnowledgeResetToolEnabled
              else regularGetDefinitionTool
          customRunTestSuiteConfig =
            findRunTestSuiteOverride customCommandToolConfigs
          selectedRunTestSuiteTool =
            maybe runTestSuiteTool customRunTestSuiteTool customRunTestSuiteConfig
          customCommandTools =
            map customCommandTool (filter ((/= "runTestSuite") . (.name)) customCommandToolConfigs)
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
            selectedRunTestSuiteTool
          ]
            <> definitionKnowledgeTools
            <> feedbackTools
            <> customCommandTools

    findRunTestSuiteOverride :: [CustomCommandToolConfig] -> Maybe CustomCommandToolConfig
    findRunTestSuiteOverride =
      find ((== "runTestSuite") . (.name))

    filterEnabledTools :: McpConfig -> [SomeTool m] -> [SomeTool m]
    filterEnabledTools config =
      filter \tool ->
        toolEnabled config (getToolName tool)

    failWithSessionConfigError =
      ioError . userError . T.unpack . renderSessionConfigError

    failWithMcpConfigError =
      ioError . userError . T.unpack . renderMcpConfigError
