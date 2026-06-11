{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Mcp.Protocol.Request
  ( parseMcpRequest,
    parseToolCallParams,
    McpRequest (..),
    McpRequest'Notification (..),
    McpRequest'Tools (..),
  )
where

import Data.Aeson (Value)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Lore.JsonRpc.Server
  ( JsonRpcRequest (..),
  )

data McpRequest
  = Initialize
  | Ping
  | Notification McpRequest'Notification
  | Tools McpRequest'Tools
  | OtherRequest Text (Maybe Value)
  deriving (Show, Eq)

data McpRequest'Notification
  = Initialized
  | OtherNotification Text
  deriving (Show, Eq)

data McpRequest'Tools
  = ToolsList
  | ToolsCall Text (Maybe Value)
  deriving (Show, Eq)

parseMcpRequest :: JsonRpcRequest -> Either Text McpRequest
parseMcpRequest JsonRpcRequest {jsonRpcMethod, jsonRpcParams} =
  case jsonRpcMethod of
    "initialize" ->
      Right Initialize
    "ping" ->
      Right Ping
    "tools/list" ->
      Right $ Tools ToolsList
    "tools/call" -> withParams $ \params -> do
      (name, args) <- parseToolCallParams "tools/call" params
      Right $ Tools (ToolsCall name args)
    "notifications/initialized" ->
      Right $ Notification Initialized
    otherMethod
      | isNotification ->
          Right $ Notification (OtherNotification otherMethod)
    otherMethod ->
      Right $ OtherRequest otherMethod jsonRpcParams
  where
    isNotification = "notifications/" `T.isPrefixOf` jsonRpcMethod
    withParams f = case jsonRpcParams of
      Nothing -> Left $ "method " <> jsonRpcMethod <> " requires params"
      Just params -> f params

parseToolCallParams :: Text -> Value -> Either Text (Text, Maybe Value)
parseToolCallParams methodName (J.Object obj) = do
  name <- case KM.lookup "name" obj of
    Just (J.String t) -> Right t
    Just _ -> Left (methodName <> ".params.name must be a string")
    Nothing -> Left (methodName <> ".params.name is missing")
  let args = KM.lookup "arguments" obj
  Right (name, args)
parseToolCallParams methodName _ =
  Left (methodName <> " params must be an object")
