module Lore.Mcp.Internal.Tool where

import Control.Lens ((%~), (&), (.~), (^.))
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import Data.Maybe (catMaybes)
import Data.OpenApi (ToSchema, toInlinedSchema)
import qualified Data.OpenApi as OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Lore.Mcp.Internal.Annotated (FieldType (..))

data ToolWithArgs m r = ToolWithArgs
  { name :: Text,
    description :: Maybe Text,
    handler :: r 'ValueType -> m Text
  }

data ToolWithoutArgs m = ToolWithoutArgs
  { name :: Text,
    description :: Maybe Text,
    handler :: m Text
  }

data SomeTool m where
  SomeToolWithArgs :: (ToSchema (r 'MetadataType), J.FromJSON (r 'ValueType)) => ToolWithArgs m r -> SomeTool m
  SomeToolWithoutArgs :: ToolWithoutArgs m -> SomeTool m

getToolArgsInputSchema :: forall r m. (ToSchema (r 'MetadataType)) => ToolWithArgs m r -> J.Value
getToolArgsInputSchema _tool =
  J.toJSON (moveFieldsAnnotationsIntoDescription (toInlinedSchema @(r 'MetadataType) Proxy))

getToolName :: SomeTool m -> Text
getToolName = \case
  SomeToolWithArgs tool -> tool.name
  SomeToolWithoutArgs tool -> tool.name

getToolDescription :: SomeTool m -> Maybe Text
getToolDescription = \case
  SomeToolWithArgs tool -> tool.description
  SomeToolWithoutArgs tool -> tool.description

getSomeToolSpec :: SomeTool m -> J.Value
getSomeToolSpec someTool =
  J.object
    [ "name" J..= getToolName someTool,
      "description" J..= getToolDescription someTool,
      "inputSchema" J..= toolInputSchema
    ]
  where
    toolInputSchema = case someTool of
      SomeToolWithArgs (_tool :: ToolWithArgs m r) -> J.toJSON (moveFieldsAnnotationsIntoDescription $ toInlinedSchema @(r 'MetadataType) Proxy)
      SomeToolWithoutArgs _ ->
        J.object
          [ "type" J..= ("object" :: Text),
            "properties" J..= J.object [],
            "additionalProperties" J..= False
          ]

moveFieldsAnnotationsIntoDescription :: OpenApi.Schema -> OpenApi.Schema
moveFieldsAnnotationsIntoDescription schema =
  let formatDescription = case schema ^. OpenApi.format of
        Nothing -> Nothing
        Just format' -> Just $ "format: " <> renderAsText format'
      minimumDescription = case schema ^. OpenApi.minimum_ of
        Nothing -> Nothing
        Just minimum' -> Just $ "minimum: " <> renderAsText minimum'
      maximumDescription = case schema ^. OpenApi.maximum_ of
        Nothing -> Nothing
        Just maximum' -> Just $ "maximum: " <> renderAsText maximum'
      minItemsDescription = case schema ^. OpenApi.minItems of
        Nothing -> Nothing
        Just minItems' -> Just $ "minItems: " <> renderAsText minItems'
      maxItemsDescription = case schema ^. OpenApi.maxItems of
        Nothing -> Nothing
        Just maxItems' -> Just $ "maxItems: " <> renderAsText maxItems'
      exampleDescription = case schema ^. OpenApi.example of
        Nothing -> Nothing
        Just example' -> Just $ "example: " <> renderAsText example'
      baseDescription = schema ^. OpenApi.description
      newDescription =
        case catMaybes [baseDescription, formatDescription, minimumDescription, maximumDescription, minItemsDescription, maxItemsDescription, exampleDescription] of
          [] -> Nothing
          xs -> Just $ T.intercalate "\n" xs
      tweakItems :: Maybe OpenApi.OpenApiItems -> Maybe OpenApi.OpenApiItems
      tweakItems = \case
        Just (OpenApi.OpenApiItemsObject properties) -> Just $ OpenApi.OpenApiItemsObject $ fmap moveFieldsAnnotationsIntoDescription properties
        Just (OpenApi.OpenApiItemsArray items) -> Just $ OpenApi.OpenApiItemsArray $ fmap (fmap moveFieldsAnnotationsIntoDescription) items
        Nothing -> Nothing
   in schema
        & OpenApi.format .~ Nothing
        & OpenApi.minimum_ .~ Nothing
        & OpenApi.maximum_ .~ Nothing
        & OpenApi.description .~ newDescription
        & OpenApi.properties %~ fmap (fmap moveFieldsAnnotationsIntoDescription)
        & OpenApi.allOf %~ fmap (fmap (fmap moveFieldsAnnotationsIntoDescription))
        & OpenApi.anyOf %~ fmap (fmap (fmap moveFieldsAnnotationsIntoDescription))
        & OpenApi.oneOf %~ fmap (fmap (fmap moveFieldsAnnotationsIntoDescription))
        & OpenApi.items %~ tweakItems
  where
    renderAsText :: (J.ToJSON a) => a -> Text
    renderAsText = TE.decodeUtf8 . LBS.toStrict . J.encode
