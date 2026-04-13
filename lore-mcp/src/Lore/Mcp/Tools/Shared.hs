module Lore.Mcp.Tools.Shared
  ( appendPartialLoadWarning,
    PaginatedDefinitionModules (..),
    paginationSummaryLines,
    renderPaginatedDefinitionModules,
    renderDiagnosticSummary,
    renderFailureWithPartialLoadWarning,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Lore (DefinitionSlice (..), LoadTargetsResult (..), mergeDefinitionSlices, renderDefinitionModuleText)
import Lore.Diagnostics (Diagnostic (..))
import Lore.Mcp.Tools.Shared.Diagnostics (renderDiagnosticSummary)

appendPartialLoadWarning :: LoadTargetsResult -> Text -> Text -> Text
appendPartialLoadWarning loadResult partialLoadSuffix body
  | loadResult.loadTargetsModulesFailed > 0 =
      body
        <> "\n\n"
        <> renderPartialLoadWarning loadResult partialLoadSuffix
  | otherwise =
      body

renderFailureWithPartialLoadWarning :: LoadTargetsResult -> Text -> Text -> [Diagnostic] -> Text
renderFailureWithPartialLoadWarning loadResult partialLoadSuffix heading diagnostics =
  appendPartialLoadWarning loadResult partialLoadSuffix renderedBody
  where
    renderedBody =
      T.unlines $
        [heading]
          <> case diagnostics of
            [] -> ["- No diagnostics were produced."]
            _ -> map renderDiagnosticSummary diagnostics

renderPartialLoadWarning :: LoadTargetsResult -> Text -> Text
renderPartialLoadWarning loadResult partialLoadSuffix =
  "Warning: only "
    <> T.pack (show loadResult.loadTargetsModulesLoaded)
    <> " of "
    <> T.pack (show loadResult.loadTargetsModulesTotal)
    <> " modules loaded successfully. "
    <> partialLoadSuffix

data PaginatedDefinitionModules = PaginatedDefinitionModules
  { totalItems :: Int,
    skippedItems :: Int,
    shownItems :: Int,
    renderedPage :: Maybe Text
  }

paginationSummaryLines :: Text -> Text -> PaginatedDefinitionModules -> [Text]
paginationSummaryLines itemLabel skipArgName paginatedDefinitions
  | paginatedDefinitions.skippedItems == 0
      && paginatedDefinitions.shownItems == paginatedDefinitions.totalItems =
      ["Showing all " <> T.pack (show paginatedDefinitions.totalItems) <> " " <> itemLabel <> "."]
  | otherwise =
      showingLine
        <> overflowLine
  where
    showingLine =
      [ "Showing "
          <> T.pack (show paginatedDefinitions.shownItems)
          <> " of "
          <> T.pack (show paginatedDefinitions.totalItems)
          <> " "
          <> itemLabel
          <> skippedSuffix
          <> "."
      ]

    skippedSuffix
      | paginatedDefinitions.skippedItems > 0 =
          ", after skipping "
            <> T.pack (show paginatedDefinitions.skippedItems)
      | otherwise =
          ""

    overflowLine
      | remainingItems > 0 =
          [ "And "
              <> T.pack (show remainingItems)
              <> " more "
              <> itemLabel
              <> " (set "
              <> skipArgName
              <> " to "
              <> T.pack (show nextSkip)
              <> " to get the next page if required)."
          ]
      | otherwise =
          []

    remainingItems =
      paginatedDefinitions.totalItems
        - paginatedDefinitions.skippedItems
        - paginatedDefinitions.shownItems

    nextSkip =
      paginatedDefinitions.skippedItems + paginatedDefinitions.shownItems

renderPaginatedDefinitionModules :: Int -> Int -> [DefinitionSlice] -> IO (Maybe PaginatedDefinitionModules)
renderPaginatedDefinitionModules skip maxItems definitionSlices =
  case expandDefinitionSlices definitionSlices of
    [] ->
      pure Nothing
    expandedSlices -> do
      let totalItems = length expandedSlices
          skippedItems = min skip totalItems
          visibleSlices = take maxItems (drop skippedItems expandedSlices)
          renderedSlices = mergeDefinitionModules visibleSlices
      renderedPage <-
        case renderedSlices of
          [] ->
            pure Nothing
          _ ->
            Just . T.intercalate "\n\n" <$> mapM renderDefinitionModuleText renderedSlices
      pure $
        Just
          PaginatedDefinitionModules
            { totalItems,
              skippedItems,
              shownItems = length visibleSlices,
              renderedPage
            }

expandDefinitionSlices :: [DefinitionSlice] -> [DefinitionSlice]
expandDefinitionSlices =
  concatMap expandDefinitionSlice . mergeDefinitionModules

expandDefinitionSlice :: DefinitionSlice -> [DefinitionSlice]
expandDefinitionSlice definitionSlice =
  [ definitionSlice {declarationSpans = [definitionSpans]}
  | definitionSpans <- definitionSlice.declarationSpans
  ]

mergeDefinitionModules :: [DefinitionSlice] -> [DefinitionSlice]
mergeDefinitionModules =
  Map.elems . foldl insertSlice Map.empty
  where
    insertSlice acc slice =
      Map.insertWith mergeTwo slice.definitionModule slice acc

    mergeTwo new old =
      case mergeDefinitionSlices [old, new] of
        Just merged ->
          merged
        Nothing ->
          old
