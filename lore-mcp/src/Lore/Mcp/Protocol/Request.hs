{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Mcp.Protocol.Request
  ( parseMcpRequest,
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
  | OtherRequest Text
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
      (name, args) <- parseToolsCall params
      Right $ Tools (ToolsCall name args)
    "notifications/initialized" ->
      Right $ Notification Initialized
    otherMethod
      | isNotification ->
          Right $ Notification (OtherNotification otherMethod)
    otherMethod ->
      Right $ OtherRequest otherMethod
  where
    isNotification = "notifications/" `T.isPrefixOf` jsonRpcMethod
    withParams f = case jsonRpcParams of
      Nothing -> Left $ "method " <> jsonRpcMethod <> " requires params"
      Just params -> f params

    parseToolsCall :: Value -> Either Text (Text, Maybe Value)
    parseToolsCall (J.Object obj) = do
      name <- case KM.lookup "name" obj of
        Just (J.String t) -> Right t
        Just _ -> Left "tools/call.params.name must be a string"
        Nothing -> Left "tools/call.params.name is missing"
      let args = KM.lookup "arguments" obj
      Right (name, args)
    parseToolsCall _ =
      Left "tools/call params must be an object"
