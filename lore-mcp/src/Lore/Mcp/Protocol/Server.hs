module Lore.Mcp.Protocol.Server
  ( CustomRequestHandler (..),
    ExecutedToolResult (..),
    McpServer (..),
    McpServerState,
    executeToolCall,
    handleMcpRequest,
    initialMcpServerState,
    runMcpServer,
  )
where

import Control.Exception (SomeException, evaluate)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import Lore.Mcp.Internal.Tool (DynamicTool (..), SomeTool (..), ToolWithArgs (..), ToolWithoutArgs (..), getSomeToolSpec, getToolName)
import Lore.Mcp.Protocol.Request (McpRequest (..), McpRequest'Notification (..), McpRequest'Tools (..), parseMcpRequest)
import Lore.Mcp.Version (loreVersionText)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import UnliftIO (MonadUnliftIO, try)

newtype McpServerState = McpServerState
  { mcpServerInitialized :: IORef Bool
  }

initialMcpServerState :: IO McpServerState
initialMcpServerState = do
  ref <- newIORef False
  pure $ McpServerState {mcpServerInitialized = ref}

newtype CustomRequestHandler m = CustomRequestHandler
  { runCustomRequestHandler :: Maybe Value -> m (Either JsonRpcError Value)
  }

data McpServer m = McpServer
  { name :: Text,
    initialize :: m (),
    tools :: [SomeTool m],
    customRequestHandlers :: Map Text (CustomRequestHandler m),
    renderer :: LoreDoc -> Text
  }

data ExecutedToolResult = ExecutedToolResult
  { executedToolContent :: Text,
    executedToolStructuredContent :: Maybe Value
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
  Log.debug "Finished handling request"
  pure res

handleMcpRequest :: forall m. (MonadUnliftIO m) => McpServerState -> McpServer m -> McpRequest -> m JsonRpcResponse
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
  Tools (ToolsCall toolName toolArgs) -> withInitializedServer do
    executedResult <- executeToolCall server.tools server.renderer toolName toolArgs
    pure $
      case executedResult of
        Left jsonRpcError ->
          JsonRpcErrorResponse jsonRpcError
        Right output ->
          JsonRpcResult (toolCallResult output)
  OtherRequest method params ->
    withInitializedServer (handleCustomRequest method params)
  where
    withInitializedServer action = do
      isServerInitialized <- liftIO $ readIORef state.mcpServerInitialized
      if isServerInitialized
        then action
        else pure $ JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode = -32002, jsonRpcErrorMessage = "server not initialized"}
    toolsSpecs = map getSomeToolSpec server.tools
    handleCustomRequest method params =
      case Map.lookup method server.customRequestHandlers of
        Nothing ->
          pure $ JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode = -32601, jsonRpcErrorMessage = "method not found: " <> method}
        Just customHandler -> do
          customResult <-
            try @_ @SomeException (runCustomRequestHandler customHandler params)
          case customResult of
            Left exception ->
              pure $ JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode = -32603, jsonRpcErrorMessage = "internal error: " <> T.pack (show exception)}
            Right (Left jsonRpcError) ->
              pure $ JsonRpcErrorResponse jsonRpcError
            Right (Right value) ->
              pure $ JsonRpcResult value

executeToolCall ::
  forall m.
  (MonadUnliftIO m) =>
  [SomeTool m] ->
  (LoreDoc -> Text) ->
  Text ->
  Maybe Value ->
  m (Either JsonRpcError ExecutedToolResult)
executeToolCall tools render toolName toolArgs =
  case find ((== toolName) . getToolName) tools of
    Nothing ->
      pure (Left (invalidParamsError ("tool not found: " <> toolName)))
    Just someTool -> do
      toolResult <- executeSomeTool someTool
      pure (first invalidParamsError toolResult)
  where
    executeSomeTool :: SomeTool m -> m (Either Text ExecutedToolResult)
    executeSomeTool someTool =
      case someTool of
        SomeToolWithArgs ToolWithArgs {handler = toolHandler} ->
          case decodeArguments toolArgs of
            Left err ->
              pure (Left err)
            Right parsedArgs ->
              runToolHandler (toolHandler parsedArgs) Nothing
        SomeToolWithArgsStructured ToolWithArgs {handler = toolHandler} structuredProjection ->
          case decodeArguments toolArgs of
            Left err ->
              pure (Left err)
            Right parsedArgs ->
              runToolHandler (toolHandler parsedArgs) (Just (structuredProjection parsedArgs))
        SomeToolWithoutArgs ToolWithoutArgs {handler = toolHandler} ->
          runToolHandler toolHandler Nothing
        SomeToolWithoutArgsStructured ToolWithoutArgs {handler = toolHandler} structuredProjection ->
          runToolHandler toolHandler (Just structuredProjection)
        SomeDynamicTool DynamicTool {handler = toolHandler} ->
          case toolArgs of
            Nothing ->
              pure (Left "missing arguments")
            Just rawArguments ->
              runToolHandler (toolHandler rawArguments) Nothing

    decodeArguments :: forall args. (J.FromJSON args) => Maybe Value -> Either Text args
    decodeArguments maybeArguments =
      case maybeArguments of
        Nothing ->
          Left "missing arguments"
        Just rawArguments ->
          case J.fromJSON rawArguments of
            J.Error errorMessage ->
              Left ("invalid arguments: " <> T.pack errorMessage)
            J.Success parsedArguments ->
              Right parsedArguments

    runToolHandler :: forall output. (ToLoreDoc output) => m output -> Maybe (output -> Value) -> m (Either Text ExecutedToolResult)
    runToolHandler action maybeStructuredProjection =
      first (T.pack . show) <$> try @_ @SomeException do
        output <- action
        let rendered = render (toLoreDoc output)
            structuredContent = fmap ($ output) maybeStructuredProjection
        _ <- liftIO (evaluate (T.length rendered))
        case structuredContent of
          Nothing ->
            pure ()
          Just value -> do
            let encoded = J.encode value
            _ <- liftIO (evaluate (LBS.length encoded))
            pure ()
        pure
          ExecutedToolResult
            { executedToolContent = rendered,
              executedToolStructuredContent = structuredContent
            }

initializeResult :: Text -> Value
initializeResult serverName =
  object
    [ "protocolVersion" .= ("2024-11-05" :: Text),
      "serverInfo"
        .= object
          [ "name" .= serverName,
            "version" .= loreVersionText
          ],
      "capabilities"
        .= object
          [ "tools" .= object []
          ]
    ]

toolCallResult :: ExecutedToolResult -> Value
toolCallResult output =
  object
    [ "content"
        .= [ object
               [ "type" .= ("text" :: Text),
                 "text" .= output.executedToolContent
               ]
           ],
      "isError" .= False
    ]

invalidParamsError :: Text -> JsonRpcError
invalidParamsError message =
  JsonRpcError
    { jsonRpcErrorCode = -32602,
      jsonRpcErrorMessage = message
    }
