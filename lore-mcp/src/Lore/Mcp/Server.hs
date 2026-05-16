module Lore.Mcp.Server
  ( runLoreMcpServer,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Text as T
import Lore (LogLevel (..), LoggerHandle, ParallelWorkersCount (..), SessionConfig (..), noLogHandle, prettyLoggerHandle)
import Lore.Mcp.Monad (newLoreMcpContext, runLoreMcp)
import Lore.Mcp.Protocol.Server (McpServer (..), runMcpServer)
import Lore.Mcp.Tools.CreateTemporalModule (createTemporalModuleTool)
import Lore.Mcp.Tools.DiscoverDirectory (discoverDirectoryTool)
import Lore.Mcp.Tools.DiscoverProject (discoverProjectTool)
import Lore.Mcp.Tools.ExecuteCode (executeCodeTool)
import Lore.Mcp.Tools.Feedback (feedbackTool)
import Lore.Mcp.Tools.FindReferences (findReferencesTool)
import Lore.Mcp.Tools.GetDefinition.Cached (cachedGetDefinitionTool)
import Lore.Mcp.Tools.GetDefinition.Regular (regularGetDefinitionTool)
import Lore.Mcp.Tools.GetTypeOfExpression (getTypeOfExpressionTool)
import Lore.Mcp.Tools.ListExportedSymbols (listExportedSymbolsTool)
import Lore.Mcp.Tools.LookupInstances (lookupInstancesTool)
import Lore.Mcp.Tools.LookupSymbolInfo (lookupSymbolInfoTool)
import Lore.Mcp.Tools.NotifyKnowledgeReset (notifyKnowledgeResetTool)
import Lore.Mcp.Tools.ReloadHomeModules (reloadHomeModulesTool)
import Lore.Mcp.Tools.SearchSymbols (searchSymbolsTool)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

runLoreMcpServer :: IO ()
runLoreMcpServer = do
  sessionConfig <- resolveSessionConfig
  definitionKnowledgeCacheEnabled <- fromMaybe False <$> lookupOptionalEnvParsed "LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE" parseBool
  mcpContext <- newLoreMcpContext definitionKnowledgeCacheEnabled
  maybeFeedbackFilePath <- lookupEnv "LORE_MCP_FEEDBACK_FILE"
  runLoreMcp sessionConfig mcpContext do
    let feedbackTools =
          case maybeFeedbackFilePath of
            Just feedbackFilePath
              | not (null feedbackFilePath) ->
                  [feedbackTool feedbackFilePath]
            _ ->
              []
        definitionKnowledgeTools =
          if definitionKnowledgeCacheEnabled
            then [cachedGetDefinitionTool, notifyKnowledgeResetTool]
            else [regularGetDefinitionTool]
    runMcpServer
      McpServer
        { name = "lore",
          initialize = pure (),
          tools =
            [ reloadHomeModulesTool,
              createTemporalModuleTool,
              executeCodeTool,
              getTypeOfExpressionTool,
              lookupSymbolInfoTool,
              searchSymbolsTool,
              listExportedSymbolsTool,
              lookupInstancesTool,
              findReferencesTool,
              discoverProjectTool,
              discoverDirectoryTool
            ]
              <> definitionKnowledgeTools
              <> feedbackTools
        }
  where
    defaultSessionConfig =
      SessionConfig
        { projectRoot = ".",
          ghcWorkDir = ".lore-work",
          loggerHandle = noLogHandle,
          customPrelude = Nothing,
          parallelWorkersLimit = WorkersAsNumProcessors
        }
    resolveSessionConfig = do
      projectRootOverride <- lookupEnv "LORE_MCP_PROJECT_ROOT"
      ghcWorkDirOverride <- lookupEnv "LORE_MCP_GHC_WORK_DIR"
      customPreludeOverride <- lookupOptionalEnvParsed "LORE_MCP_CUSTOM_PRELUDE" parseCustomPrelude
      parallelWorkersLimitOverride <- lookupOptionalEnvParsed "LORE_MCP_PARALLEL_WORKERS_LIMIT" parseParallelWorkersCount
      loggerHandleOverride <- resolveLoggerHandle
      pure
        defaultSessionConfig
          { projectRoot = fromMaybe (projectRoot defaultSessionConfig) projectRootOverride,
            ghcWorkDir = fromMaybe (ghcWorkDir defaultSessionConfig) ghcWorkDirOverride,
            loggerHandle = loggerHandleOverride,
            customPrelude = customPreludeOverride <|> customPrelude defaultSessionConfig,
            parallelWorkersLimit = fromMaybe (parallelWorkersLimit defaultSessionConfig) parallelWorkersLimitOverride
          }

lookupOptionalEnvParsed :: String -> (String -> Maybe a) -> IO (Maybe a)
lookupOptionalEnvParsed envName parseValue = do
  maybeRawValue <- lookupEnv envName
  case maybeRawValue of
    Nothing -> pure Nothing
    Just rawValue ->
      case parseValue rawValue of
        Just value -> pure (Just value)
        Nothing -> ioError $ userError ("Invalid value for " <> envName <> ": " <> show rawValue)

parseCustomPrelude :: String -> Maybe T.Text
parseCustomPrelude rawValue
  | T.null normalized = Nothing
  | otherwise = Just normalized
  where
    normalized = T.strip (pack rawValue)

parseParallelWorkersCount :: String -> Maybe ParallelWorkersCount
parseParallelWorkersCount rawValue
  | rawValue == "auto" = Just WorkersAsNumProcessors
  | otherwise = do
      workersCount <- readMaybe rawValue
      if workersCount > 0
        then Just (ThisWorkersCount workersCount)
        else Nothing

resolveLoggerHandle :: IO LoggerHandle
resolveLoggerHandle = do
  maybeLogLevel <- lookupOptionalEnvParsed "LORE_MCP_LOG_LEVEL" parseLogLevel
  pure $
    case maybeLogLevel of
      Just logLevel -> prettyLoggerHandle logLevel
      Nothing -> noLogHandle

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel rawValue =
  case T.toLower (T.strip (pack rawValue)) of
    "debug" -> Just Debug
    "info" -> Just Info
    "warning" -> Just Warning
    "warn" -> Just Warning
    "error" -> Just Error
    _ -> Nothing

parseBool :: String -> Maybe Bool
parseBool rawValue =
  case T.toLower (T.strip (pack rawValue)) of
    "1" -> Just True
    "true" -> Just True
    "yes" -> Just True
    "on" -> Just True
    "0" -> Just False
    "false" -> Just False
    "no" -> Just False
    "off" -> Just False
    _ -> Nothing
