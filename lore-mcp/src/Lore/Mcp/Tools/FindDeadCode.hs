module Lore.Mcp.Tools.FindDeadCode
  ( findDeadCodeTool,
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
    ExampleList,
    Field,
    FieldType (..),
    WithMeta,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Tools.FindDeadCode
  ( FindDeadCodeOptions (..),
    findDeadCode,
    renderFindDeadCodeOutput,
  )
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.Result (PageRequest (..), ResultLimit (..), ToolRun (..))

data FindDeadCodeArgs (fieldType :: FieldType) = FindDeadCodeArgs
  { modules ::
      Maybe (Field fieldType [Text])
        `WithMeta` '[ Description "Only report dead definitions from these loaded home modules.",
                      ExampleList '["Demo", "Demo.Support"]
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial dead definitions to skip.",
                      Example 25
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (FindDeadCodeArgs 'ValueType)

instance ToSchema (FindDeadCodeArgs 'MetadataType)

findDeadCodeTool :: (MonadLore m) => SomeTool m
findDeadCodeTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "findDeadCode",
        description = Just "Find top-level dead declarations using project-wide reachability over cached definition indexes.",
        handler = findDeadCodeHandler
      }

findDeadCodeHandler :: (MonadLore m) => FindDeadCodeArgs 'ValueType -> m LoreDoc
findDeadCodeHandler FindDeadCodeArgs {modules, skip} = do
  result <-
    findDeadCode
      FindDeadCodeOptions
        { findDeadCodeModules = modules,
          findDeadCodePageRequest =
            Just
              PageRequest
                { pageOffset = max 0 (maybe 0 id skip),
                  pageLimit = Limit (limitToIntWithDefault 100 (deadCodeLimit mcpDefaultToolPolicy))
                }
        }
  pure $
    case result of
      ToolRunBlocked blocked ->
        toLoreDoc blocked
      ToolRunReady output ->
        renderFindDeadCodeOutput output
