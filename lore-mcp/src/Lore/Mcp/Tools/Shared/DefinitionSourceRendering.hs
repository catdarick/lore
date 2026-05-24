module Lore.Mcp.Tools.Shared.DefinitionSourceRendering
  ( PaginatedDefinitionSources (..),
    buildDefinitionSourceFiles,
    buildPaginatedDefinitionSourceFiles,
    paginateDefinitionSources,
  )
where

-- Shared MCP rendering for Lore DefinitionSource values.
-- Used by getDefinition and resolveInstance.

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (foldl', sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import Lore
  ( DeclarationSpans (..),
    DefinitionId (..),
    DefinitionSource (..),
    NamedDefinitionSource (..),
  )
import Lore.Definition.RenderSlice (definitionSourceToRenderSlice)
import Lore.Mcp.Internal.LoreDoc (SourceFile)
import Lore.Mcp.Tools.Shared (Paginated (..))
import Lore.Mcp.Tools.Shared.Source (definitionSlicesToSourceFiles)

buildPaginatedDefinitionSourceFiles ::
  (MonadIO m) =>
  Int ->
  Int ->
  [NamedDefinitionSource] ->
  m (Maybe (Paginated SourceFile))
buildPaginatedDefinitionSourceFiles skip maxItems definitionEntries =
  case paginateDefinitionSources skip maxItems definitionEntries of
    Nothing ->
      pure Nothing
    Just paginatedSources ->
      if null paginatedSources.visibleDefinitionSources
        then
          pure
            ( Just
                Paginated
                  { paginatedTotalItems = paginatedSources.sourceTotalItems,
                    paginatedSkippedItems = paginatedSources.sourceSkippedItems,
                    paginatedShownItems = 0,
                    paginatedConsumedItems = 0,
                    paginatedItems = []
                  }
            )
        else do
          renderedFiles <- buildDefinitionSourceFiles paginatedSources.visibleDefinitionSources
          pure
            ( Just
                Paginated
                  { paginatedTotalItems = paginatedSources.sourceTotalItems,
                    paginatedSkippedItems = paginatedSources.sourceSkippedItems,
                    paginatedShownItems = length paginatedSources.visibleDefinitionSources,
                    paginatedConsumedItems = length paginatedSources.visibleDefinitionSources,
                    paginatedItems = renderedFiles
                  }
            )

buildDefinitionSourceFiles ::
  (MonadIO m) =>
  [NamedDefinitionSource] ->
  m [SourceFile]
buildDefinitionSourceFiles definitionEntries =
  liftIO (definitionSlicesToSourceFiles visibleSlices)
  where
    visibleSlices =
      map (definitionSourceToRenderSlice . (.definitionSource)) definitionEntries

data PaginatedDefinitionSources = PaginatedDefinitionSources
  { sourceTotalItems :: !Int,
    sourceSkippedItems :: !Int,
    visibleDefinitionSources :: ![NamedDefinitionSource]
  }

paginateDefinitionSources :: Int -> Int -> [NamedDefinitionSource] -> Maybe PaginatedDefinitionSources
paginateDefinitionSources skip maxItems definitionEntries =
  case sortedSources of
    [] ->
      Nothing
    _ ->
      Just
        PaginatedDefinitionSources
          { sourceTotalItems = totalItems,
            sourceSkippedItems = skippedItems,
            visibleDefinitionSources = take maxItems (drop skippedItems sortedSources)
          }
  where
    sortedSources =
      sortOn definitionSourceSortKey (dedupeDefinitionSources definitionEntries)
    totalItems =
      length sortedSources
    skippedItems =
      min skip totalItems

dedupeDefinitionSources :: [NamedDefinitionSource] -> [NamedDefinitionSource]
dedupeDefinitionSources =
  reverse . snd . foldl' dedupeOne (Set.empty, [])
  where
    dedupeOne (seenDefinitionIds, deduped) definitionEntry
      | Set.member definitionId seenDefinitionIds =
          (seenDefinitionIds, deduped)
      | otherwise =
          (Set.insert definitionId seenDefinitionIds, definitionEntry : deduped)
      where
        definitionId =
          definitionEntry.definitionSource.definitionSourceId

definitionSourceSortKey :: NamedDefinitionSource -> (String, String, Int, Int, Text)
definitionSourceSortKey definitionEntry =
  case GHC.srcSpanToRealSrcSpan definitionEntry.definitionSource.definitionSourceSpans.declarationSpan of
    Just realSpan ->
      ( moduleName,
        GHC.unpackFS (GHC.srcSpanFile realSpan),
        GHC.srcSpanStartLine realSpan,
        GHC.srcSpanStartCol realSpan,
        definitionIdSortKey definitionEntry.definitionSource.definitionSourceId
      )
    Nothing ->
      ( moduleName,
        "",
        maxBound,
        maxBound,
        definitionIdSortKey definitionEntry.definitionSource.definitionSourceId
      )
  where
    moduleName =
      GHC.moduleNameString (GHC.moduleName definitionEntry.definitionSource.definitionSourceModule)

definitionIdSortKey :: DefinitionId -> Text
definitionIdSortKey definitionId =
  T.pack (show definitionId.definitionIdSpanKey)
