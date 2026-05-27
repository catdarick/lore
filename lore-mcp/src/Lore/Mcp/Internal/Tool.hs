module Lore.Mcp.Internal.Tool where

import Control.Lens ((%~), (&), (.~), (?~), (^.))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JKM
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import qualified Data.HashMap.Strict.InsOrd as IOM
import Data.Maybe (catMaybes)
import Data.OpenApi (ToSchema, toInlinedSchema)
import qualified Data.OpenApi as OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Lore.Mcp.Internal.Annotated (FieldType (..))
import Lore.Tools.Render.Doc (ToLoreDoc)

data ToolWithArgs m r output = ToolWithArgs
  { name :: Text,
    description :: Maybe Text,
    handler :: r 'ValueType -> m output
  }

data ToolWithoutArgs m output = ToolWithoutArgs
  { name :: Text,
    description :: Maybe Text,
    handler :: m output
  }

data SomeTool m where
  SomeToolWithArgs ::
    (ToSchema (r 'MetadataType), J.FromJSON (r 'ValueType), ToLoreDoc output) =>
    ToolWithArgs m r output ->
    SomeTool m
  SomeToolWithoutArgs ::
    (ToLoreDoc output) =>
    ToolWithoutArgs m output ->
    SomeTool m

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
      SomeToolWithArgs (_tool :: ToolWithArgs m r output) ->
        openApiNullableToJsonSchemaNullable $
          J.toJSON $
            moveFieldsAnnotationsIntoDescription $
              toInlinedSchema @(r 'MetadataType) Proxy
      SomeToolWithoutArgs _ ->
        J.object
          [ "type" J..= ("object" :: Text),
            "properties" J..= J.object [],
            "additionalProperties" J..= False
          ]

-- | Convert OpenAPI 3.0-style nullable schemas:
--
--   {
--     "type": "string",
--     "nullable": true
--   }
--
-- into JSON Schema-style nullable schemas:
--
--   {
--     "type": ["string", "null"]
--   }
--
-- This is useful for OpenAI tool / structured-output schemas.
openApiNullableToJsonSchemaNullable :: J.Value -> J.Value
openApiNullableToJsonSchemaNullable = \case
  J.Object object ->
    let processedObject =
          fmap openApiNullableToJsonSchemaNullable object

        nullable =
          JKM.lookup "nullable" processedObject == Just (J.Bool True)

        withoutNullable =
          JKM.delete "nullable" processedObject
     in if nullable
          then J.Object $ addNullToType withoutNullable
          else J.Object withoutNullable
  J.Array values ->
    J.Array $ fmap openApiNullableToJsonSchemaNullable values
  value ->
    value

addNullToType :: J.Object -> J.Object
addNullToType object =
  case JKM.lookup "type" object of
    Just (J.String typeName) ->
      JKM.insert
        (JK.fromString "type")
        (J.Array $ V.fromList [J.String typeName, J.String "null"])
        object
    Just (J.Array types) ->
      let hasNull =
            J.String "null" `elem` V.toList types

          newTypes =
            if hasNull
              then types
              else V.snoc types (J.String "null")
       in JKM.insert
            (JK.fromString "type")
            (J.Array newTypes)
            object
    -- If there is no "type", we cannot safely rewrite it as a type union.
    -- For example, schemas using oneOf/anyOf/allOf may not have a direct type.
    -- In that case, just remove nullable and leave the schema otherwise intact.
    _ ->
      object

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
        case catMaybes
          [ baseDescription,
            formatDescription,
            minimumDescription,
            maximumDescription,
            minItemsDescription,
            maxItemsDescription,
            exampleDescription
          ] of
          [] -> Nothing
          xs -> Just $ T.intercalate "\n" xs

      requiredNames = schema ^. OpenApi.required
      allProperties = IOM.keys $ schema ^. OpenApi.properties

      makeNullable :: OpenApi.Schema -> OpenApi.Schema
      makeNullable =
        OpenApi.nullable ?~ True

      tweakProperty ::
        Text ->
        OpenApi.Referenced OpenApi.Schema ->
        OpenApi.Referenced OpenApi.Schema
      tweakProperty name prop =
        let prop' = fmap moveFieldsAnnotationsIntoDescription prop
         in if name `elem` requiredNames
              then prop'
              else fmap makeNullable prop'

      tweakItems :: Maybe OpenApi.OpenApiItems -> Maybe OpenApi.OpenApiItems
      tweakItems = \case
        Just (OpenApi.OpenApiItemsObject properties) ->
          Just $
            OpenApi.OpenApiItemsObject $
              fmap moveFieldsAnnotationsIntoDescription properties
        Just (OpenApi.OpenApiItemsArray items) ->
          Just $
            OpenApi.OpenApiItemsArray $
              fmap (fmap moveFieldsAnnotationsIntoDescription) items
        Nothing ->
          Nothing
   in schema
        & OpenApi.format .~ Nothing
        & OpenApi.minimum_ .~ Nothing
        & OpenApi.maximum_ .~ Nothing
        & OpenApi.description .~ newDescription
        -- OpenAI strict-schema compatibility:
        -- originally optional properties become required-but-nullable.
        & OpenApi.required .~ allProperties
        & OpenApi.properties %~ IOM.mapWithKey tweakProperty
        & OpenApi.allOf %~ fmap (fmap (fmap moveFieldsAnnotationsIntoDescription))
        & OpenApi.anyOf %~ fmap (fmap (fmap moveFieldsAnnotationsIntoDescription))
        & OpenApi.oneOf %~ fmap (fmap (fmap moveFieldsAnnotationsIntoDescription))
        & OpenApi.items %~ tweakItems
  where
    renderAsText :: (J.ToJSON a) => a -> Text
    renderAsText = TE.decodeUtf8 . LBS.toStrict . J.encode
