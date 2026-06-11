module Lore.Mcp.StructuredToolRpc
  ( structuredToolRequestHandlers,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Lore.JsonRpc.Server (JsonRpcError (..))
import Lore.Mcp.Internal.Tool (SomeTool)
import Lore.Mcp.Protocol.Request (parseToolCallParams)
import Lore.Mcp.Protocol.Server
  ( CustomRequestHandler (..),
    ExecutedToolResult (..),
    executeToolCall,
  )
import Lore.Tools.Render.Doc (LoreDoc)
import UnliftIO (MonadUnliftIO)

structuredToolRequestHandlers ::
  (MonadUnliftIO m) =>
  [SomeTool m] ->
  (LoreDoc -> Text) ->
  Map Text (CustomRequestHandler m)
structuredToolRequestHandlers tools render =
  Map.singleton
    "lore/tools/callStructured"
    (CustomRequestHandler (handleStructuredToolCall tools render))

handleStructuredToolCall ::
  (MonadUnliftIO m) =>
  [SomeTool m] ->
  (LoreDoc -> Text) ->
  Maybe Value ->
  m (Either JsonRpcError Value)
handleStructuredToolCall tools render maybeParams =
  case parseStructuredToolParams maybeParams of
    Left jsonRpcError ->
      pure (Left jsonRpcError)
    Right (toolName, toolArgs) ->
      fmap structuredToolCallEnvelope <$> executeToolCall tools render toolName toolArgs

parseStructuredToolParams :: Maybe Value -> Either JsonRpcError (Text, Maybe Value)
parseStructuredToolParams maybeParams =
  case maybeParams of
    Nothing ->
      Left (invalidParamsError "method tools/call requires params")
    Just params ->
      case parseToolCallParams "tools/call" params of
        Left err ->
          Left (invalidParamsError err)
        Right parsed ->
          Right parsed

structuredToolCallEnvelope :: ExecutedToolResult -> Value
structuredToolCallEnvelope output =
  object
    [ "content"
        .= [ object
               [ "type" .= ("text" :: Text),
                 "text" .= output.executedToolContent
               ]
           ],
      "isError" .= False,
      "structuredContent" .= output.executedToolStructuredContent
    ]

invalidParamsError :: Text -> JsonRpcError
invalidParamsError message =
  JsonRpcError
    { jsonRpcErrorCode = -32602,
      jsonRpcErrorMessage = message
    }
