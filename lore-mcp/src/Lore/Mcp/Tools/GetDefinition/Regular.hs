module Lore.Mcp.Tools.GetDefinition.Regular
  ( regularGetDefinitionTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore, NamedDefinitionSource (..))
import Lore.Mcp.Internal.Annotated (Description, Example, ExampleList, Field, FieldType (..), Maximum, MinItems, Minimum, WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.GetDefinition.Shared
  ( CommonGetDefinitionArgs (..),
    FilteredDefinitions (..),
    defaultRecursionDepth,
    getDefinitionHandlerWithStrategy,
    maxRenderedDefinitionResults,
    renderPaginatedDefinitionSources,
  )

data GetDefinitionArgs (fieldType :: FieldType) = GetDefinitionArgs
  { symbols ::
      Field fieldType [Text]
        `WithMeta` '[ Description "Exact symbol names to resolve and render definitions for. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      ExampleList '["HasIndex", "mkIndexed", "Some.Module.someFunction"],
                      MinItems 1
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 30
                    ],
    recursionDepth ::
      Field fieldType (Maybe Int)
        `WithMeta` '[ Description "Maximum recursive definition depth. Defaults to 0. If greater than 0, definitions will be resolved recursively to the specified depth, where 1 means only directly referenced definitions will be included, 2 means definitions directly referenced by those definitions will also be included, and so on.",
                      Example 2,
                      Minimum 0,
                      Maximum 20
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (GetDefinitionArgs 'ValueType)

instance ToSchema (GetDefinitionArgs 'MetadataType)

regularGetDefinitionTool :: (MonadLore m) => SomeTool m
regularGetDefinitionTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "getDefinition",
        description = Just "Render source definitions for one or more exported symbols when source is available. Use recursionDepth to include referenced definitions. Returned imports are minified and may not exactly match original module import formatting. This can still succeed usefully during partial load if the requested definition is available.",
        handler = regularGetDefinitionHandler
      }

regularGetDefinitionHandler :: (MonadLore m) => GetDefinitionArgs 'ValueType -> m Text
regularGetDefinitionHandler GetDefinitionArgs {symbols, skip, recursionDepth} =
  getDefinitionHandlerWithStrategy commonArgs renderWithoutKnowledgeCache
  where
    commonArgs =
      CommonGetDefinitionArgs
        { symbols,
          skip,
          recursionDepth = Just (max 0 (fromMaybeDefault defaultRecursionDepth recursionDepth))
        }

renderWithoutKnowledgeCache ::
  (MonadLore m) =>
  Int ->
  [NamedDefinitionSource] ->
  m FilteredDefinitions
renderWithoutKnowledgeCache skip definitionEntries = do
  renderedDefinitions <-
    renderPaginatedDefinitionSources
      skip
      maxRenderedDefinitionResults
      definitionEntries
  pure
    FilteredDefinitions
      { renderedDefinitions,
        omittedKnownDefinitions = [],
        omittedKnownDefinitionCount = 0
      }

fromMaybeDefault :: a -> Maybe a -> a
fromMaybeDefault fallback = \case
  Just value -> value
  Nothing -> fallback
