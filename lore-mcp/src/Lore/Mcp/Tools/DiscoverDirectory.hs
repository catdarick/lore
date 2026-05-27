module Lore.Mcp.Tools.DiscoverDirectory
  ( discoverDirectoryTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Tools.DiscoverDirectory
  ( DiscoverDirectoryOptions (..),
    DiscoverDirectoryRenderMode (..),
    discoverDirectory,
    renderDiscoverDirectory,
  )
import Lore.Tools.Pagination (ToolPolicy (..), limitToMaybeInt, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))

data DiscoverDirectoryArgs (fieldType :: FieldType) = DiscoverDirectoryArgs
  { path ::
      Field fieldType FilePath
        `WithMeta` '[ Description "Path of directory to discover, relative to the project root.",
                      Example ".",
                      Example "src",
                      Example "src/features"
                    ],
    depth ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "depth=0 lists the requested directory and its immediate entries. depth=1 also lists direct child directories. depth=2 also lists grandchildren directories.",
                      Example 0,
                      Example 1,
                      Example 2
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (DiscoverDirectoryArgs 'ValueType)

instance ToSchema (DiscoverDirectoryArgs 'MetadataType)

discoverDirectoryTool :: (MonadLore m) => SomeTool m
discoverDirectoryTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "discoverDirectory",
        description = Just "Discover and render a directory tree recursively. It has depth and item limiters to avoid bloating the context window.",
        handler = discoverDirectoryHandler
      }

newtype DiscoverDirectoryResult = DiscoverDirectoryResult
  { discoverDirectoryRendered :: LoreDoc
  }

instance ToLoreDoc DiscoverDirectoryResult where
  toLoreDoc = (.discoverDirectoryRendered)

discoverDirectoryHandler :: (MonadLore m) => DiscoverDirectoryArgs 'ValueType -> m DiscoverDirectoryResult
discoverDirectoryHandler DiscoverDirectoryArgs {path, depth} = do
  output <- discoverDirectory options
  pure (DiscoverDirectoryResult (renderDiscoverDirectory renderMode output))
  where
    options =
      DiscoverDirectoryOptions
        { discoverDirectoryPath = path,
          discoverDirectoryDepth = resolvedDepth,
          discoverDirectoryBudget = limitToMaybeInt (directoryEntryBudget mcpDefaultToolPolicy)
        }
    renderMode =
      if resolvedDepth == Just 0
        then DiscoverDirectoryRenderCompact
        else DiscoverDirectoryRenderTree
    resolvedDepth =
      fmap (max 0) depth
