module Lore.Mcp.Tools.GetDefinition.Regular
  ( regularGetDefinitionTool,
  )
where

import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (FieldType (..))
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.GetDefinition.Shared
  ( GetDefinitionArgs,
    GetDefinitionResult,
    maxRenderedDefinitionResults,
    mkOmittedDefinitions,
    toGetDefinitionRequest,
    toGetDefinitionResult,
  )
import Lore.Tools.GetDefinition
  ( BuildDefinitionsStrategy,
    FilteredDefinitions (..),
    getDefinitionHandlerWithStrategy,
  )
import Lore.Tools.Internal.DefinitionSourceRendering (buildPaginatedDefinitionSourceFiles)
import Lore.Tools.Result (PageRequest (..), ResultLimit (..))

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
  do
    coreResult <-
      getDefinitionHandlerWithStrategy
        (toGetDefinitionRequest args)
        buildWithoutKnowledgeCache
    pure (toGetDefinitionResult False coreResult)

buildWithoutKnowledgeCache ::
  (MonadLore m) =>
  BuildDefinitionsStrategy m
buildWithoutKnowledgeCache pageRequest _directlyRequestedSymbolNames definitionEntries = do
  let maxItems =
        case pageRequest.pageLimit of
          Unlimited -> maxRenderedDefinitionResults
          Limit requestedLimit -> min maxRenderedDefinitionResults (max 0 requestedLimit)
  filteredDefinitionPage <-
    buildPaginatedDefinitionSourceFiles
      pageRequest.pageOffset
      maxItems
      definitionEntries
  pure
    FilteredDefinitions
      { filteredDefinitionPage,
        filteredOmittedDefinitions = mkOmittedDefinitions []
      }
