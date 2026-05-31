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
    Example,
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
        `WithMeta` '[ Description "Class application to resolve.",
                      Example "Render (Maybe Foo)",
                      Example "Show Bar",
                      Example "TwoTypeClass TypeOne TypeTwo"
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
        description = Just "Resolve the class instance. When source is available, the tool renders the selected instance definition; otherwise it returns the selected instance head and defining module.",
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
