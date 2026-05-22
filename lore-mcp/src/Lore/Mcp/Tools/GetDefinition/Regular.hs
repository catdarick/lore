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
        description = Just "Render source definitions for one or more symbols when source is available.",
        handler = regularGetDefinitionHandler
      }

regularGetDefinitionHandler :: (MonadLore m) => GetDefinitionArgs 'ValueType -> m GetDefinitionResult
regularGetDefinitionHandler args =
  getDefinitionHandlerWithStrategy False args buildWithoutKnowledgeCache

buildWithoutKnowledgeCache ::
  (MonadLore m) =>
  BuildDefinitionsStrategy m
buildWithoutKnowledgeCache skip _directlyRequestedSymbolNames definitionEntries = do
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
