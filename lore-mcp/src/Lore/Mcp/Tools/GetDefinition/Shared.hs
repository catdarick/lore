module Lore.Mcp.Tools.GetDefinition.Shared
  ( CommonGetDefinitionArgs (..),
    FilteredDefinitions (..),
    RenderDefinitionsStrategy,
    defaultRecursionDepth,
    maxRenderedDefinitionResults,
    getDefinitionHandlerWithStrategy,
    renderPaginatedDefinitionSources,
    PaginatedDefinitionSources (..),
    paginateDefinitionSources,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import Lore
  ( DeclarationSpans (..),
    DefinitionId (..),
    DefinitionSource (..),
    MonadLore,
    NamedDefinitionSource (..),
    Symbol (..),
    SymbolInfo (..),
    getMinifiedImportsForDefinition,
    lookupLastLoadHomeModulesResult,
    lookupSymbolInfo,
    resolveDefinitionClosureSourcesNamed,
    resolveDefinitionSourceNamed,
  )
import Lore.Definition.RenderSlice (definitionSourceToRenderSlice)
import Lore.Mcp.Tools.Shared (PaginatedDefinitionModules (..), appendPartialLoadWarning, paginationSummaryLines)
import qualified Lore.Mcp.Tools.Shared as Shared
import Lore.Mcp.Tools.Shared.SymbolResolution (ResolvedSymbolQuery (resolvedSymbol), withResolvedSymbols)

data CommonGetDefinitionArgs = CommonGetDefinitionArgs
  { symbols :: [Text],
    skip :: Maybe Int,
    recursionDepth :: Maybe Int
  }

data FilteredDefinitions = FilteredDefinitions
  { renderedDefinitions :: Maybe PaginatedDefinitionModules,
    omittedKnownDefinitions :: [GHC.Name],
    omittedKnownDefinitionCount :: Int
  }

type RenderDefinitionsStrategy m =
  Int ->
  [NamedDefinitionSource] ->
  m FilteredDefinitions

getDefinitionHandlerWithStrategy :: (MonadLore m) => Bool -> CommonGetDefinitionArgs -> RenderDefinitionsStrategy m -> m Text
getDefinitionHandlerWithStrategy shouldRenderNotifyKnowledgeResetHint CommonGetDefinitionArgs {symbols, skip, recursionDepth} renderDefinitions = do
  maybeLoadResult <- lookupLastLoadHomeModulesResult
  case maybeLoadResult of
    Nothing ->
      pure "Home modules have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      renderedBody <-
        withResolvedSymbols symbols \resolvedQueries -> do
          resolvedSymbolInfos <-
            catMaybes
              <$> mapM
                (lookupSymbolInfo . (.name) . (.resolvedSymbol))
                resolvedQueries
          definitionEntries <- concat <$> mapM (resolveSymbolDefinitions resolvedRecursionDepth) resolvedSymbolInfos
          filteredDefinitions <- renderDefinitions resolvedSkip definitionEntries
          pure (renderDefinitionResult shouldRenderNotifyKnowledgeResetHint symbols filteredDefinitions)
      pure (appendPartialLoadWarning loadResult "Definition results may be incomplete." renderedBody)
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)
    resolvedRecursionDepth =
      max 0 (fromMaybe defaultRecursionDepth recursionDepth)

defaultRecursionDepth :: Int
defaultRecursionDepth = 0

resolveSymbolDefinitions :: (MonadLore m) => Int -> SymbolInfo -> m [NamedDefinitionSource]
resolveSymbolDefinitions recursionDepth symbolInfo
  | recursionDepth == 0 =
      maybe [] (pure . NamedDefinitionSource symbolInfo.symbolName) <$> resolveDefinitionSourceNamed symbolInfo.symbolName
  | otherwise =
      resolveDefinitionClosureSourcesNamed recursionDepth symbolInfo.symbolName

renderDefinitionResult :: Bool -> [Text] -> FilteredDefinitions -> Text
renderDefinitionResult shouldRenderNotifyKnowledgeResetHint symbols renderedDefinitions =
  T.intercalate "\n\n" (renderDefinitionSections shouldRenderNotifyKnowledgeResetHint symbols renderedDefinitions)

renderDefinitionSections :: Bool -> [Text] -> FilteredDefinitions -> [Text]
renderDefinitionSections shouldRenderNotifyKnowledgeResetHint symbols filteredDefinitions =
  case filteredDefinitions.renderedDefinitions of
    Nothing
      | filteredDefinitions.omittedKnownDefinitionCount > 0 ->
          allDefinitionsOmittedSection shouldRenderNotifyKnowledgeResetHint filteredDefinitions
      | otherwise ->
          ["No definitions found for " <> quoteTexts symbols <> "."]
    Just paginatedDefinitions ->
      definitionResultsSection paginatedDefinitions
        <> omittedDefinitionsSection shouldRenderNotifyKnowledgeResetHint filteredDefinitions

allDefinitionsOmittedSection :: Bool -> FilteredDefinitions -> [Text]
allDefinitionsOmittedSection shouldRenderNotifyKnowledgeResetHint filteredDefinitions =
  [ T.intercalate "\n" $
      [ "All matching definitions are completely UNCHANGED and were omitted from this tool response to optimize token usage. They are already fresh and valid inside your current context window:"
      ]
        <> omittedDefinitionsDetailLines shouldRenderNotifyKnowledgeResetHint filteredDefinitions
  ]

omittedDefinitionsSection :: Bool -> FilteredDefinitions -> [Text]
omittedDefinitionsSection shouldRenderNotifyKnowledgeResetHint filteredDefinitions
  | count <= 0 = []
  | otherwise =
      [ T.intercalate "\n" $
          [ "The following definitions are completely UNCHANGED and were omitted from this tool response to optimize token usage. They are already fresh and valid inside your current context window:"
          ]
            <> omittedDefinitionsDetailLines shouldRenderNotifyKnowledgeResetHint filteredDefinitions
      ]
  where
    count = filteredDefinitions.omittedKnownDefinitionCount

omittedDefinitionsDetailLines :: Bool -> FilteredDefinitions -> [Text]
omittedDefinitionsDetailLines shouldRenderNotifyKnowledgeResetHint filteredDefinitions =
  omittedDefinitionLines filteredDefinitions.omittedKnownDefinitions
    <> notifyKnowledgeResetHintLines shouldRenderNotifyKnowledgeResetHint

notifyKnowledgeResetHintLines :: Bool -> [Text]
notifyKnowledgeResetHintLines shouldRenderNotifyKnowledgeResetHint
  | shouldRenderNotifyKnowledgeResetHint =
      ["IF AND ONLY IF your active conversation history was just wiped, or you have suffered a total memory reset and literally cannot see these definitions in your previous turns, you should execute the `notifyKnowledgeReset` tool to resync the server cache."]
  | otherwise =
      []

omittedDefinitionLines :: [GHC.Name] -> [Text]
omittedDefinitionLines omittedDefinitions =
  map (("  - " <>) . renderModuleOmittedSymbolsLine) groupedDefinitions
  where
    groupedDefinitions = sortOn fst (groupOmittedDefinitionsByModule omittedDefinitions)

groupOmittedDefinitionsByModule :: [GHC.Name] -> [(Text, [Text])]
groupOmittedDefinitionsByModule names =
  Map.toList $
    foldl' collectDefinition Map.empty names
  where
    collectDefinition grouped name =
      Map.insertWith (<>) (definitionModuleName name) [definitionSymbolName name] grouped

definitionModuleName :: GHC.Name -> Text
definitionModuleName name =
  case GHC.nameModule_maybe name of
    Just module_ -> T.pack (GHC.moduleNameString (GHC.moduleName module_))
    Nothing -> "<unknown module>"

definitionSymbolName :: GHC.Name -> Text
definitionSymbolName =
  T.pack . GHC.getOccString

renderModuleOmittedSymbolsLine :: (Text, [Text]) -> Text
renderModuleOmittedSymbolsLine (moduleName, symbolNames) =
  moduleName <> ": " <> renderedSymbols
  where
    dedupedSymbols = dedupeTexts symbolNames
    shownSymbols = take maxRenderedOmittedSymbolsPerModule dedupedSymbols
    hiddenCount = length dedupedSymbols - length shownSymbols
    baseRenderedSymbols = T.intercalate ", " shownSymbols
    renderedSymbols
      | hiddenCount > 0 =
          baseRenderedSymbols
            <> " and "
            <> T.pack (show hiddenCount)
            <> " more"
      | otherwise =
          baseRenderedSymbols

dedupeTexts :: [Text] -> [Text]
dedupeTexts =
  reverse . snd . foldl' dedupeText (Set.empty, [])
  where
    dedupeText (seenTexts, deduped) value
      | Set.member value seenTexts =
          (seenTexts, deduped)
      | otherwise =
          (Set.insert value seenTexts, value : deduped)

definitionResultsSection :: PaginatedDefinitionModules -> [Text]
definitionResultsSection paginatedDefinitions =
  paginationSummaryLines "definition results" "skip" paginatedDefinitions
    <> maybe [] pure (renderPage paginatedDefinitions)

renderPage :: PaginatedDefinitionModules -> Maybe Text
renderPage paginatedDefinitions =
  case paginatedDefinitions.renderedPage of
    Just page -> Just page
    Nothing -> Nothing

renderPaginatedDefinitionSources ::
  (MonadLore m) =>
  Int ->
  Int ->
  [NamedDefinitionSource] ->
  m (Maybe PaginatedDefinitionModules)
renderPaginatedDefinitionSources skip maxItems definitionEntries =
  case paginateDefinitionSources skip maxItems definitionEntries of
    Nothing ->
      pure Nothing
    Just paginatedSources -> do
      visibleSlices <- mapM renderSource paginatedSources.visibleDefinitionSources
      if null visibleSlices
        then
          pure $
            Just
              PaginatedDefinitionModules
                { totalItems = paginatedSources.sourceTotalItems,
                  skippedItems = paginatedSources.sourceSkippedItems,
                  shownItems = 0,
                  renderedPage = Just ""
                }
        else do
          renderedDefinitions <-
            liftIO $
              Shared.renderPaginatedDefinitionModules
                0
                maxItems
                visibleSlices
          pure $
            fmap
              ( \rendered ->
                  rendered
                    { totalItems = paginatedSources.sourceTotalItems,
                      skippedItems = paginatedSources.sourceSkippedItems,
                      shownItems = length paginatedSources.visibleDefinitionSources
                    }
              )
              renderedDefinitions
  where
    renderSource definitionEntry = do
      imports <- getMinifiedImportsForDefinition definitionEntry.definitionSource
      pure (definitionSourceToRenderSlice definitionEntry.definitionSource imports)

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

quoteTexts :: [Text] -> Text
quoteTexts values =
  "[" <> T.intercalate ", " (map (\value -> "\"" <> value <> "\"") values) <> "]"

maxRenderedDefinitionResults :: Int
maxRenderedDefinitionResults = 30

maxRenderedOmittedSymbolsPerModule :: Int
maxRenderedOmittedSymbolsPerModule = 10
