module Lore.Mcp.Protocol.Server
  ( McpServer (..),
    runMcpServer,
  )
where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.JsonRpc.Server
  ( JsonRpcError (..),
    JsonRpcHandlerResult (..),
    JsonRpcRequest (..),
    JsonRpcResponse (..),
    runJsonRpcLoop,
  )
import qualified Lore.Logger as Log
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), ToolWithoutArgs (..), getSomeToolSpec, getToolName)
import Lore.Mcp.Protocol.Request (McpRequest (..), McpRequest'Notification (..), McpRequest'Tools (..), parseMcpRequest)
import UnliftIO (MonadUnliftIO, try)

newtype McpServerState = McpServerState
  { mcpServerInitialized :: IORef Bool
  }

initialMcpServerState :: IO McpServerState
initialMcpServerState = do
  ref <- newIORef False
  pure $ McpServerState {mcpServerInitialized = ref}

data McpServer m = McpServer
  { name :: Text,
    initialize :: m (),
    tools :: [SomeTool m]
  }

runMcpServer :: (MonadUnliftIO m, Log.MonadLogger m) => McpServer m -> m ()
runMcpServer mcpServer = do
  state <- liftIO initialMcpServerState
  runJsonRpcLoop (handleJsonRpcRequest state mcpServer)

handleJsonRpcRequest :: (MonadUnliftIO m, Log.MonadLogger m) => McpServerState -> McpServer m -> JsonRpcRequest -> m JsonRpcHandlerResult
handleJsonRpcRequest state server jsonRpcRequest = do
  Log.debug $ "Got JSON-RPC request: " <> T.unpack (jsonRpcMethod jsonRpcRequest) <> " with params: " <> BL8.unpack (J.encode jsonRpcRequest.jsonRpcParams)
  res <- case parseMcpRequest jsonRpcRequest of
    Left err ->
      pure
        JsonRpcHandlerResult
          { jsonRpcHandlerResponse = JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode = -32602, jsonRpcErrorMessage = "invalid request: " <> err},
            jsonRpcShouldExit = False
          }
    Right mcpRequest -> do
      response <- handleMcpRequest state server mcpRequest
      pure $ JsonRpcHandlerResult {jsonRpcHandlerResponse = response, jsonRpcShouldExit = False}
  Log.debug $ "Finished handling request"
  pure res

handleMcpRequest :: (MonadUnliftIO m) => McpServerState -> McpServer m -> McpRequest -> m JsonRpcResponse
handleMcpRequest state server mcpRequest = case mcpRequest of
  Initialize -> do
    server.initialize
    liftIO $ writeIORef state.mcpServerInitialized True
    pure $ JsonRpcResult (initializeResult server.name)
  Notification Initialized ->
    pure JsonRpcNoResponse
  Notification (OtherNotification _) ->
    pure JsonRpcNoResponse
  Ping ->
    pure $ JsonRpcResult (object [])
  Tools ToolsList -> withInitializedServer do
    pure $ JsonRpcResult (object ["tools" .= toolsSpecs])
  Tools (ToolsCall name args) -> withInitializedServer do
    withTool name $ \someTool -> do
      eiOutput <- case someTool of
        SomeToolWithArgs tool -> do
          case J.fromJSON <$> args of
            Nothing -> pure $ Left "missing arguments"
            Just (J.Error e) -> pure $ Left ("invalid arguments: " <> T.pack e)
            Just (J.Success parsedArgs) -> first (T.pack . show) <$> try @_ @SomeException (tool.handler parsedArgs)
        SomeToolWithoutArgs tool -> do
          first (T.pack . show) <$> try @_ @SomeException tool.handler
      case eiOutput of
        Left err -> pure $ JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode = -32602, jsonRpcErrorMessage = err}
        Right output -> pure $ JsonRpcResult (toolCallResult output)
  OtherRequest method ->
    pure $ JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode = -32601, jsonRpcErrorMessage = "method not found: " <> method}
  where
    withInitializedServer action = do
      isServerInitialized <- liftIO $ readIORef state.mcpServerInitialized
      if isServerInitialized
        then action
        else pure $ JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode = -32002, jsonRpcErrorMessage = "server not initialized"}
    toolsSpecs = map getSomeToolSpec server.tools
    withTool name action = do
      case find (\someTool -> name == getToolName someTool) server.tools of
        Just someTool -> action someTool
        Nothing -> pure $ JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode = -32602, jsonRpcErrorMessage = "tool not found: " <> name}

initializeResult :: Text -> Value
initializeResult serverName =
  object
    [ "protocolVersion" .= ("2024-11-05" :: Text),
      "serverInfo"
        .= object
          [ "name" .= serverName,
            "version" .= ("0.1.0.0" :: Text)
          ],
      "capabilities"
        .= object
          [ "tools" .= object []
          ]
    ]

toolCallResult :: Text -> Value
toolCallResult output =
  object
    [ "content"
        .= [ object
               [ "type" .= ("text" :: Text),
                 "text" .= output
               ]
           ],
      "isError" .= False
    ]
