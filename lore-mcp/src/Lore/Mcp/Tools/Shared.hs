module Lore.Mcp.Tools.Shared
  ( appendPartialLoadWarning,
    PaginatedDefinitionSlices (..),
    PaginatedDefinitionModules (..),
    paginateDefinitionSlices,
    paginationSummaryLines,
    renderDeclarationBodyText,
    renderPaginatedDefinitionModules,
    renderDiagnosticSummary,
    renderFailureWithPartialLoadWarning,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import Lore (DeclarationSpans (..), DefinitionSlice (..), LoadTargetsResult (..), mergeDefinitionSlices)
import Lore.Diagnostics (Diagnostic (..))
import Lore.Mcp.Internal.List (maximumMaybe, minimumMaybe)
import Lore.Mcp.Internal.SourceSpan (realSrcSpanFromSrcSpan)
import Lore.Mcp.Internal.SourceText (readSpanText, relativeSourcePath)
import Lore.Mcp.Tools.Shared.Diagnostics (renderDiagnosticSummary)
import System.Directory (getCurrentDirectory)

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

data PaginatedDefinitionSlices = PaginatedDefinitionSlices
  { sliceTotalItems :: Int,
    sliceSkippedItems :: Int,
    visibleSlices :: [DefinitionSlice]
  }

renderDefinitionModuleText :: DefinitionSlice -> IO Text
renderDefinitionModuleText definitionSlice = do
  renderedPath <- renderDefinitionModulePath definitionSlice
  renderedDeclarations <- mapM renderDeclarationBlock (sortDeclarationSpans definitionSlice.declarationSpans)
  let renderedBlocks =
        filter (not . T.null) renderedDeclarations
  pure $
    T.intercalate "\n\n" $
      ["=== " <> renderedPath <> " ==="]
        <> renderedBlocks

renderDefinitionModulePath :: DefinitionSlice -> IO Text
renderDefinitionModulePath definitionSlice =
  case definitionSliceRealSrcSpan definitionSlice of
    Nothing ->
      pure "<definition source unavailable>"
    Just realSrcSpan -> do
      currentDirectory <- liftIO getCurrentDirectory
      pure . T.pack $
        relativeSourcePath currentDirectory (GHC.unpackFS (GHC.srcSpanFile realSrcSpan))

definitionSliceRealSrcSpan :: DefinitionSlice -> Maybe GHC.RealSrcSpan
definitionSliceRealSrcSpan definitionSlice =
  case mapMaybe declarationSpansRealSrcSpan definitionSlice.declarationSpans of
    realSrcSpan : _ -> Just realSrcSpan
    [] -> Nothing

declarationSpansRealSrcSpan :: DeclarationSpans -> Maybe GHC.RealSrcSpan
declarationSpansRealSrcSpan spans =
  realSrcSpanFromSrcSpan spans.declarationSpan
    <|> (spans.signatureSpan >>= realSrcSpanFromSrcSpan)

renderDeclarationBlock :: DeclarationSpans -> IO Text
renderDeclarationBlock declarationSpans = do
  declarationText <- renderDeclarationSpansText declarationSpans
  pure $
    T.intercalate
      "\n"
      [ "--- " <> renderDeclarationBlockHeader declarationSpans <> " ---",
        declarationText
      ]

renderDeclarationSpansText :: DeclarationSpans -> IO Text
renderDeclarationSpansText spans = do
  declarationText <- readSpanText spans.declarationSpan
  signatureText <- traverse readSpanText spans.signatureSpan
  pure $
    maybe declarationText (<> "\n" <> declarationText) signatureText

renderDeclarationBodyText :: DeclarationSpans -> IO Text
renderDeclarationBodyText spans =
  readSpanText spans.declarationSpan

renderDeclarationBlockHeader :: DeclarationSpans -> Text
renderDeclarationBlockHeader declarationSpans =
  case declarationSpansLineRange declarationSpans of
    Nothing ->
      "definition"
    Just (startLine, endLine) ->
      "lines " <> T.pack (show startLine) <> "-" <> T.pack (show endLine)

declarationSpansLineRange :: DeclarationSpans -> Maybe (Int, Int)
declarationSpansLineRange declarationSpans = do
  firstSpan <- minimumMaybe realSrcSpans
  lastSpan <- maximumMaybe realSrcSpans
  pure (GHC.srcSpanStartLine firstSpan, GHC.srcSpanEndLine lastSpan)
  where
    realSrcSpans =
      mapMaybe realSrcSpanFromSrcSpan $
        maybeToList declarationSpans.signatureSpan <> [declarationSpans.declarationSpan]

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  sortOn (realSrcSpanFromSrcSpan . declarationSpan)

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
  case paginateDefinitionSlices skip maxItems definitionSlices of
    Nothing ->
      pure Nothing
    Just paginatedSlices -> do
      renderedPage <- T.intercalate "\n\n" <$> mapM renderDefinitionModuleText (mergeDefinitionModules visibleSlices)
      pure $
        Just
          PaginatedDefinitionModules
            { totalItems = paginatedSlices.sliceTotalItems,
              skippedItems = paginatedSlices.sliceSkippedItems,
              shownItems = length paginatedSlices.visibleSlices,
              renderedPage = Just renderedPage
            }
      where
        visibleSlices = paginatedSlices.visibleSlices

paginateDefinitionSlices :: Int -> Int -> [DefinitionSlice] -> Maybe PaginatedDefinitionSlices
paginateDefinitionSlices skip maxItems definitionSlices =
  case expandDefinitionSlices definitionSlices of
    [] ->
      Nothing
    expandedSlices ->
      Just $
        PaginatedDefinitionSlices
          { sliceTotalItems = totalItems,
            sliceSkippedItems = skippedItems,
            visibleSlices = take maxItems (drop skippedItems expandedSlices)
          }
      where
        totalItems = length expandedSlices
        skippedItems = min skip totalItems

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
