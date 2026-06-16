module Lore.Mcp.Tools.ResolveInstance
  ( resolveInstanceTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated
  ( Description,
    Field,
    FieldType (..),
    WithMeta,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), renderToolRun)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.ResolveInstance
  ( ResolveInstanceOptions (..),
    resolveInstance,
  )

data ResolveInstanceArgs (fieldType :: FieldType) = ResolveInstanceArgs
  { query ::
      Field fieldType Text
        `WithMeta` '[ Description "Class application to resolve. Examples: \"Render (Maybe Foo)\", \"Show Bar\", \"TwoTypeClass TypeOne TypeTwo\"."
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (ResolveInstanceArgs 'ValueType)

instance ToSchema (ResolveInstanceArgs 'MetadataType)

resolveInstanceTool :: (MonadLore m) => SomeTool m
resolveInstanceTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "resolveInstance",
        description = Just "Resolve the specific typeclass instance selected by GHC for a concrete class application, such as Render (Maybe Foo). Use this when you need to know which instance dictionary applies to a particular type. When project source is available, the selected instance declaration is rendered; otherwise the instance head and defining module are returned. Use lookupInstances instead to search broadly for indexed instance declarations mentioning several names.",
        handler = resolveInstanceHandler
      }

resolveInstanceHandler :: (MonadLore m) => ResolveInstanceArgs 'ValueType -> m LoreDoc
resolveInstanceHandler ResolveInstanceArgs {query} = do
  result <-
    resolveInstance
      ResolveInstanceOptions
        { resolveInstanceQuery = query
        }
  pure $ renderToolRun toLoreDoc result
