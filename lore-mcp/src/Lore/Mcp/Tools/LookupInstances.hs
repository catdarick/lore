module Lore.Mcp.Tools.LookupInstances
  ( lookupInstancesTool,
  )
where

import qualified Data.Aeson as J
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated
  ( Description,
    Example,
    ExampleList,
    Field,
    FieldType (..),
    MinItems,
    WithMeta,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), renderToolRun)
import Lore.Tools.LookupInstances
  ( LookupInstancesOptions (..),
    lookupInstances,
  )
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.Result
  ( PageRequest (..),
    ResultLimit (..)
  )

data LookupInstancesArgs (fieldType :: FieldType) = LookupInstancesArgs
  { names ::
      Field fieldType [Text]
        `WithMeta` '[ Description "Provide two or more symbol names. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      ExampleList '["Show", "Int", "Some.Module.someFunction"],
                      MinItems 2
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 5
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (LookupInstancesArgs 'ValueType)

instance ToSchema (LookupInstancesArgs 'MetadataType)

lookupInstancesTool :: (MonadLore m) => SomeTool m
lookupInstancesTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "lookupInstances",
        description = Just "Find loaded class or family instance declarations whose instance head mentions all queried symbols. This matches what is currently indexed in the loaded session; it does not infer likely instances beyond the indexed results. Example: [\"Show\", \"Int\"] matches `instance Show Int`; [\"Int\", \"String\"] matches only instances where both types appear together.",
        handler = lookupInstancesHandler
      }

lookupInstancesHandler :: (MonadLore m) => LookupInstancesArgs 'ValueType -> m LoreDoc
lookupInstancesHandler LookupInstancesArgs {names, skip} = do
  result <-
    lookupInstances
      LookupInstancesOptions
        { lookupInstancesNames = names,
          lookupInstancesPageRequest =
            PageRequest
              { pageOffset = max 0 (fromMaybe 0 skip),
                pageLimit = Limit (limitToIntWithDefault 25 (instanceLimit mcpDefaultToolPolicy))
              }
        }
  pure $ renderToolRun toLoreDoc result
