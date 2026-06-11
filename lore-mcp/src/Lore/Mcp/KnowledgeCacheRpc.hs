module Lore.Mcp.KnowledgeCacheRpc
  ( knowledgeCacheRequestHandlers,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore.JsonRpc.Server (JsonRpcError (..))
import Lore.Mcp.Monad
  ( DefinitionCacheReplacement (..),
    MonadLoreMcp,
    getSentDefinitionHashes,
    replaceSentDefinitionHashes,
  )
import Lore.Mcp.Protocol.Server (CustomRequestHandler (..))

newtype GetCachedDefinitionsResult = GetCachedDefinitionsResult
  { hashes :: [Text]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GetCachedDefinitionsResult

newtype SetCachedDefinitionsParams = SetCachedDefinitionsParams
  { hashes :: [Text]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SetCachedDefinitionsParams

newtype SetCachedDefinitionsResult = SetCachedDefinitionsResult
  { cachedDefinitionCount :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SetCachedDefinitionsResult

knowledgeCacheRequestHandlers :: (MonadLoreMcp m) => Map Text (CustomRequestHandler m)
knowledgeCacheRequestHandlers =
  Map.fromList
    [ ( "lore/knowledge/getCachedDefinitions",
        CustomRequestHandler handleGetCachedDefinitions
      ),
      ( "lore/knowledge/setCachedDefinitions",
        CustomRequestHandler handleSetCachedDefinitions
      )
    ]

handleGetCachedDefinitions :: (MonadLoreMcp m) => Maybe Value -> m (Either JsonRpcError Value)
handleGetCachedDefinitions params =
  case parseEmptyCustomRequestParams params of
    Left jsonRpcError ->
      pure (Left jsonRpcError)
    Right () -> do
      cachedHashes <- getSentDefinitionHashes
      pure $
        Right $
          J.toJSON GetCachedDefinitionsResult {hashes = Set.toAscList cachedHashes}

handleSetCachedDefinitions :: (MonadLoreMcp m) => Maybe Value -> m (Either JsonRpcError Value)
handleSetCachedDefinitions params =
  case decodeRequiredParams params of
    Left jsonRpcError ->
      pure (Left jsonRpcError)
    Right SetCachedDefinitionsParams {hashes} -> do
      replacement <- replaceSentDefinitionHashes (Set.fromList hashes)
      pure $
        Right $
          J.toJSON
            SetCachedDefinitionsResult
              { cachedDefinitionCount = replacement.currentCachedDefinitionCount
              }

parseEmptyCustomRequestParams :: Maybe Value -> Either JsonRpcError ()
parseEmptyCustomRequestParams Nothing =
  Right ()
parseEmptyCustomRequestParams (Just J.Null) =
  Right ()
parseEmptyCustomRequestParams (Just (J.Object obj))
  | KM.null obj =
      Right ()
parseEmptyCustomRequestParams _ =
  Left (invalidParamsError "expected params to be omitted, null, or an empty object")

decodeRequiredParams :: (FromJSON a) => Maybe Value -> Either JsonRpcError a
decodeRequiredParams Nothing =
  Left (invalidParamsError "missing params")
decodeRequiredParams (Just params) =
  case J.fromJSON params of
    J.Error err ->
      Left (invalidParamsError ("invalid params: " <> T.pack err))
    J.Success decoded ->
      Right decoded

invalidParamsError :: Text -> JsonRpcError
invalidParamsError message =
  JsonRpcError
    { jsonRpcErrorCode = -32602,
      jsonRpcErrorMessage = message
    }
