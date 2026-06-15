module Lore.JsonRpc.Server
  ( JsonRpcRequest (..),
    JsonRpcResponse (..),
    JsonRpcError (..),
    JsonRpcHandlerResult (..),
    runJsonRpcLoop,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Null),
    eitherDecodeStrict',
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (BufferMode (LineBuffering), hFlush, hIsEOF, hSetBuffering, stderr, stdin, stdout)

data JsonRpcRequest = JsonRpcRequest
  { jsonRpcMethod :: Text,
    jsonRpcParams :: Maybe Value,
    jsonRpcExpectsResponse :: Bool
  }
  deriving (Eq, Show)

data JsonRpcResponse
  = JsonRpcResult Value
  | JsonRpcErrorResponse JsonRpcError
  | JsonRpcNoResponse
  deriving (Eq, Show)

data JsonRpcError = JsonRpcError
  { jsonRpcErrorCode :: Int,
    jsonRpcErrorMessage :: Text
  }
  deriving (Eq, Show)

data JsonRpcHandlerResult = JsonRpcHandlerResult
  { jsonRpcHandlerResponse :: JsonRpcResponse,
    jsonRpcShouldExit :: Bool
  }

data WireJsonRpcRequest = WireJsonRpcRequest
  { wireRequestId :: Maybe Value,
    wireRequestHasId :: Bool,
    wireRequestMethod :: Text,
    wireRequestParams :: Maybe Value
  }

instance FromJSON WireJsonRpcRequest where
  parseJSON = withObject "WireJsonRpcRequest" \obj -> do
    version <- obj .:? "jsonrpc"
    wireRequestMethod <- obj .: "method"
    wireRequestParams <- obj .:? "params"
    case version of
      Nothing -> pure ()
      Just jsonrpcVersion ->
        if (jsonrpcVersion :: Text) == "2.0"
          then pure ()
          else fail "unsupported jsonrpc version"
    pure
      WireJsonRpcRequest
        { wireRequestId = KeyMap.lookup "id" obj,
          wireRequestHasId = KeyMap.member "id" obj,
          wireRequestMethod,
          wireRequestParams
        }

instance ToJSON JsonRpcError where
  toJSON JsonRpcError {jsonRpcErrorCode, jsonRpcErrorMessage} =
    object
      [ "code" .= jsonRpcErrorCode,
        "message" .= jsonRpcErrorMessage
      ]

runJsonRpcLoop :: (MonadIO m) => (JsonRpcRequest -> m JsonRpcHandlerResult) -> m ()
runJsonRpcLoop handler = do
  liftIO $ hSetBuffering stdout LineBuffering
  liftIO $ hSetBuffering stderr LineBuffering
  loop
  where
    loop = do
      eof <- liftIO $ hIsEOF stdin
      unless eof do
        input <- liftIO BS.getLine
        (response, shouldExit) <- handleIncoming input
        liftIO $ maybeSend response
        unless shouldExit loop

    handleIncoming input =
      case eitherDecodeStrict' input of
        Left decodeError ->
          pure
            ( Just (encodeWireResponse Nothing (JsonRpcErrorResponse (JsonRpcError (-32700) (T.pack decodeError)))),
              False
            )
        Right wireRequest -> do
          JsonRpcHandlerResult {jsonRpcHandlerResponse, jsonRpcShouldExit} <- handler (toStructuredRequest wireRequest)
          pure
            ( toWireResponse wireRequest jsonRpcHandlerResponse,
              jsonRpcShouldExit
            )

toStructuredRequest :: WireJsonRpcRequest -> JsonRpcRequest
toStructuredRequest WireJsonRpcRequest {wireRequestHasId, wireRequestMethod, wireRequestParams} =
  JsonRpcRequest
    { jsonRpcMethod = wireRequestMethod,
      jsonRpcParams = wireRequestParams,
      jsonRpcExpectsResponse = wireRequestHasId
    }

toWireResponse :: WireJsonRpcRequest -> JsonRpcResponse -> Maybe BL8.ByteString
toWireResponse _ JsonRpcNoResponse = Nothing
toWireResponse WireJsonRpcRequest {wireRequestHasId, wireRequestId} response
  | not wireRequestHasId = Nothing
  | otherwise = Just (encodeWireResponse wireRequestId response)

encodeWireResponse :: Maybe Value -> JsonRpcResponse -> BL8.ByteString
encodeWireResponse responseId response =
  encode $
    object $
      [ "jsonrpc" .= ("2.0" :: Text),
        "id" .= maybe Null id responseId
      ]
        ++ case response of
          JsonRpcResult result ->
            ["result" .= result]
          JsonRpcErrorResponse rpcError ->
            ["error" .= rpcError]
          JsonRpcNoResponse ->
            []

maybeSend :: Maybe BL8.ByteString -> IO ()
maybeSend Nothing = pure ()
maybeSend (Just response) = do
  BL8.putStrLn response
  hFlush stdout
