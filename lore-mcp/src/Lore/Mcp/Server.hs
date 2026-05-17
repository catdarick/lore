module Lore.Mcp.Server
  ( runLoreMcpServer,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isUpper, toUpper)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Text as T
import Lore (LogLevel (..), LoggerHandle, ParallelWorkersCount (..), SessionConfig (..), noLogHandle, prettyLoggerHandle)
import Lore.Mcp.Internal.Tool (SomeTool, getToolName)
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
import Lore.Mcp.Tools.RunTestSuite (runTestSuiteTool)
import Lore.Mcp.Tools.SearchSymbols (searchSymbolsTool)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

runLoreMcpServer :: IO ()
runLoreMcpServer = do
  sessionConfig <- resolveSessionConfig
  definitionKnowledgeCacheEnabled <- fromMaybe False <$> lookupOptionalEnvParsed "LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE" parseBool
  notifyKnowledgeResetToolEnabled <- isToolEnabledByName "notifyKnowledgeReset"
  mcpContext <- newLoreMcpContext definitionKnowledgeCacheEnabled
  maybeFeedbackFilePath <- lookupEnv "LORE_MCP_FEEDBACK_FILE"
  enabledTools <- filterEnabledTools (getTools definitionKnowledgeCacheEnabled notifyKnowledgeResetToolEnabled maybeFeedbackFilePath)
  runLoreMcp sessionConfig mcpContext do
    runMcpServer
      McpServer
        { name = "lore",
          initialize = pure (),
          tools = enabledTools
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
            findReferencesTool,
            lookupInstancesTool,
            createTemporalModuleTool,
            getTypeOfExpressionTool,
            executeCodeTool,
            runTestSuiteTool
          ]
            <> definitionKnowledgeTools
            <> feedbackTools

    filterEnabledTools :: [SomeTool m] -> IO [SomeTool m]
    filterEnabledTools tools = filterM isToolEnabled tools
      where
        isToolEnabled tool =
          isToolEnabledByName (getToolName tool)

    isToolEnabledByName :: T.Text -> IO Bool
    isToolEnabledByName toolName =
      fromMaybe True <$> lookupOptionalEnvParsed (toolEnabledEnvVarName toolName) parseBool

    toolEnabledEnvVarName :: T.Text -> String
    toolEnabledEnvVarName toolName =
      "LORE_MCP_TOOL_ENABLED_" <> toSnakeUpper (T.unpack toolName)

    toSnakeUpper :: String -> String
    toSnakeUpper raw = dropWhile (== '_') (go Nothing raw)
      where
        go _ [] = []
        go previousChar (currentChar : rest)
          | isAlphaNum currentChar =
              let boundary =
                    case previousChar of
                      Just prev
                        | isAlphaNum prev ->
                            isWordBoundary prev currentChar
                      _ ->
                        False
                  prefix = if boundary then "_" else ""
               in prefix <> [toUpper currentChar] <> go (Just currentChar) rest
          | otherwise =
              case previousChar of
                Just prev
                  | isAlphaNum prev ->
                      "_" <> go (Just '_') rest
                _ ->
                  go previousChar rest

        isWordBoundary prev current =
          (isLower prev && isUpper current)
            || (isDigit prev && isAlpha current)
            || (isAlpha prev && isDigit current)

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
