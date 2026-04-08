module Lore.Mcp.Internal.Tool where

import qualified Data.Aeson as J
import Data.Data (Proxy (..))
import Data.OpenApi (ToSchema, toInlinedSchema)
import Data.Text (Text)
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
getToolArgsInputSchema _tool = J.toJSON (toInlinedSchema @(r 'MetadataType) Proxy)

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
      SomeToolWithArgs (_tool :: ToolWithArgs m r) -> J.toJSON (toInlinedSchema @(r 'MetadataType) Proxy)
      SomeToolWithoutArgs _ ->
        J.object
          [ "type" J..= ("object" :: Text),
            "properties" J..= J.object [],
            "additionalProperties" J..= False
          ]
