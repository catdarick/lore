module Lore.Mcp.Tools.GetDefinition.Regular
  ( regularGetDefinitionTool,
  )
where

import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (FieldType (..))
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.GetDefinition.Shared
  ( BuildDefinitionsStrategy,
    FilteredDefinitions (..),
    GetDefinitionArgs,
    GetDefinitionResult,
    getDefinitionHandlerWithStrategy,
    maxRenderedDefinitionResults,
    mkOmittedDefinitions,
  )
import Lore.Mcp.Tools.Shared.DefinitionSourceRendering (buildPaginatedDefinitionSourceFiles)

regularGetDefinitionTool :: (MonadLore m) => SomeTool m
regularGetDefinitionTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "getDefinition",
        description = Just "Render source definitions for one or more exported symbols when source is available. Use expansion to control dependency inclusion: None (target only), Direct (maxDepth=1), Recursive (maxDepth=2, maxSymbols=200). Returned imports are minified and may not exactly match original module import formatting. This can still succeed usefully during partial load if the requested definition is available.",
        handler = regularGetDefinitionHandler
      }

regularGetDefinitionHandler :: (MonadLore m) => GetDefinitionArgs 'ValueType -> m GetDefinitionResult
regularGetDefinitionHandler args =
  getDefinitionHandlerWithStrategy False args buildWithoutKnowledgeCache

buildWithoutKnowledgeCache ::
  (MonadLore m) =>
  BuildDefinitionsStrategy m
buildWithoutKnowledgeCache skip definitionEntries = do
  filteredDefinitionPage <-
    buildPaginatedDefinitionSourceFiles
      skip
      maxRenderedDefinitionResults
      definitionEntries
  pure
    FilteredDefinitions
      { filteredDefinitionPage,
        filteredOmittedDefinitions = mkOmittedDefinitions []
      }
