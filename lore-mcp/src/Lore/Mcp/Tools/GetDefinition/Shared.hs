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
import Data.Either (lefts)
import Data.Function (on)
import Data.List (foldl', nubBy, sortOn)
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
    LoadTargetsResult (..),
    MonadLore,
    NamedDefinitionSource (..),
    Symbol (..),
    SymbolInfo (..),
    findMatchingSymbolsRoots,
    getMinifiedImportsForDefinition,
    lookupLastLoadTargetsResult,
    lookupSymbolInfo,
    parseAndNormalizeName,
    resolveDefinitionClosureSourcesNamed,
    resolveDefinitionSourceNamed,
  )
import Lore.Definition.RenderSlice (definitionSourceToRenderSlice)
import Lore.Mcp.Tools.Shared (PaginatedDefinitionModules (..), appendPartialLoadWarning, paginationSummaryLines)
import qualified Lore.Mcp.Tools.Shared as Shared

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

getDefinitionHandlerWithStrategy :: (MonadLore m) => CommonGetDefinitionArgs -> RenderDefinitionsStrategy m -> m Text
getDefinitionHandlerWithStrategy CommonGetDefinitionArgs {symbols, skip, recursionDepth} renderDefinitions = do
  maybeLoadResult <- lookupLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      resolution <- resolveRequestedSymbols symbols
      case resolution of
        Left (missingSymbols, ambiguousQueries) ->
          pure (renderAmbiguityResult loadResult missingSymbols ambiguousQueries)
        Right resolvedSymbols -> do
          definitionEntries <- concat <$> mapM (resolveSymbolDefinitions resolvedRecursionDepth) resolvedSymbols.resolvedSymbolInfos
          filteredDefinitions <- renderDefinitions resolvedSkip definitionEntries
          pure (renderDefinitionResult loadResult symbols resolvedSymbols.missingQueries filteredDefinitions)
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)
    resolvedRecursionDepth =
      max 0 (fromMaybe defaultRecursionDepth recursionDepth)

defaultRecursionDepth :: Int
defaultRecursionDepth = 0

data AmbiguousQuery = AmbiguousQuery
  { ambiguousQueryText :: Text,
    ambiguousQueryMatches :: [SymbolInfo]
  }

data ResolvedSymbols = ResolvedSymbols
  { missingQueries :: [Text],
    resolvedSymbolInfos :: [SymbolInfo]
  }

data ResolvedQuery
  = MissingQuery Text
  | ResolvedQuery [SymbolInfo]

resolveRequestedSymbols :: (MonadLore m) => [Text] -> m (Either ([Text], [AmbiguousQuery]) ResolvedSymbols)
resolveRequestedSymbols symbols = do
  resolvedQueries <- mapM resolveRequestedSymbol symbols
  pure $
    case lefts resolvedQueries of
      [] ->
        Right
          ResolvedSymbols
            { missingQueries = [queryText | Right (MissingQuery queryText) <- resolvedQueries],
              resolvedSymbolInfos =
                nubBy
                  ((==) `on` symbolName)
                  [ symbolInfo
                  | Right (ResolvedQuery symbolInfos) <- resolvedQueries,
                    symbolInfo <- symbolInfos
                  ]
            }
      ambiguousQueries ->
        Left
          ( [queryText | Right (MissingQuery queryText) <- resolvedQueries],
            ambiguousQueries
          )

resolveRequestedSymbol :: (MonadLore m) => Text -> m (Either AmbiguousQuery ResolvedQuery)
resolveRequestedSymbol symbol = do
  symbolInfos <- lookupRootSymbolInfos symbol
  pure $
    case symbolInfos of
      [] ->
        Right (MissingQuery symbol)
      [symbolInfo] ->
        Right (ResolvedQuery [symbolInfo])
      ambiguousMatches ->
        if allDefinedInSameModule ambiguousMatches
          then Right (ResolvedQuery ambiguousMatches)
          else
            Left
              AmbiguousQuery
                { ambiguousQueryText = symbol,
                  ambiguousQueryMatches = ambiguousMatches
                }

allDefinedInSameModule :: [SymbolInfo] -> Bool
allDefinedInSameModule symbolInfos =
  case symbolInfos of
    [] -> True
    firstSymbolInfo : restSymbolInfos ->
      all ((== firstSymbolInfo.definedIn) . (.definedIn)) restSymbolInfos

lookupRootSymbolInfos :: (MonadLore m) => Text -> m [SymbolInfo]
lookupRootSymbolInfos query = do
  rootSymbols <- Set.toList <$> findMatchingSymbolsRoots (parseAndNormalizeName query)
  catMaybes <$> mapM (lookupSymbolInfo . (.name)) rootSymbols

resolveSymbolDefinitions :: (MonadLore m) => Int -> SymbolInfo -> m [NamedDefinitionSource]
resolveSymbolDefinitions recursionDepth symbolInfo
  | recursionDepth == 0 =
      maybe [] (pure . NamedDefinitionSource symbolInfo.symbolName) <$> resolveDefinitionSourceNamed symbolInfo.symbolName
  | otherwise =
      resolveDefinitionClosureSourcesNamed recursionDepth symbolInfo.symbolName

renderDefinitionResult :: LoadTargetsResult -> [Text] -> [Text] -> FilteredDefinitions -> Text
renderDefinitionResult loadResult symbols missingSymbols renderedDefinitions =
  appendPartialLoadWarning loadResult "Definition results may be incomplete." renderedBody
  where
    renderedBody =
      T.intercalate "\n\n" $
        missingSymbolsSection missingSymbols
          <> renderDefinitionSections symbols renderedDefinitions

renderDefinitionSections :: [Text] -> FilteredDefinitions -> [Text]
renderDefinitionSections symbols filteredDefinitions =
  case filteredDefinitions.renderedDefinitions of
    Nothing
      | filteredDefinitions.omittedKnownDefinitionCount > 0 ->
          allDefinitionsOmittedSection filteredDefinitions
      | otherwise ->
          ["No definitions found for " <> quoteTexts symbols <> "."]
    Just paginatedDefinitions ->
      definitionResultsSection paginatedDefinitions
        <> omittedDefinitionsSection filteredDefinitions

allDefinitionsOmittedSection :: FilteredDefinitions -> [Text]
allDefinitionsOmittedSection filteredDefinitions =
  [ T.intercalate "\n" $
      [ "All matching definitions in this call were already returned earlier in this MCP session and were omitted now:"
      ]
        <> omittedDefinitionsDetailLines filteredDefinitions
  ]

omittedDefinitionsSection :: FilteredDefinitions -> [Text]
omittedDefinitionsSection filteredDefinitions
  | filteredDefinitions.omittedKnownDefinitionCount <= 0 =
      []
  | otherwise =
      [ T.intercalate "\n" $
          [ "Omitted "
              <> T.pack (show filteredDefinitions.omittedKnownDefinitionCount)
              <> " definition"
              <> pluralSuffix filteredDefinitions.omittedKnownDefinitionCount
              <> " that were already returned earlier in this MCP session:"
          ]
            <> omittedDefinitionsDetailLines filteredDefinitions
      ]

omittedDefinitionsDetailLines :: FilteredDefinitions -> [Text]
omittedDefinitionsDetailLines filteredDefinitions =
  omittedDefinitionLines filteredDefinitions.omittedKnownDefinitions
    <> ["Use `notifyKnowledgeReset` tool to let the server know that client knowledge has been reset to make all the definitions available by default."]

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
    Just module_ -> renderModuleName module_
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

renderAmbiguityResult :: LoadTargetsResult -> [Text] -> [AmbiguousQuery] -> Text
renderAmbiguityResult loadResult missingSymbols ambiguousQueries =
  appendPartialLoadWarning loadResult "Definition results may be incomplete." renderedBody
  where
    ambiguousCount = length ambiguousQueries
    renderedBody =
      T.intercalate "\n\n" $
        missingSymbolsSection missingSymbols
          <> [ T.intercalate "\n" $
                 [ T.pack (show ambiguousCount)
                     <> " requested name"
                     <> pluralSuffix ambiguousCount
                     <> " "
                     <> ambiguousVerb ambiguousCount
                     <> " ambiguous. More qualification is required:"
                 ]
                   <> concatMap renderAmbiguousQuery (zip [1 :: Int ..] ambiguousQueries)
                   <> ["", "Run the tool again with a qualified symbol name, for example: " <> renderExampleQualification ambiguousQueries]
             ]

renderAmbiguousQuery :: (Int, AmbiguousQuery) -> [Text]
renderAmbiguousQuery (index, ambiguousQuery) =
  ["  " <> T.pack (show index) <> ". " <> ambiguousQuery.ambiguousQueryText <> " is defined in:"]
    <> map (("       - " <>) . renderModuleName) (ambiguousDefinitionModules ambiguousQuery.ambiguousQueryMatches)

ambiguousDefinitionModules :: [SymbolInfo] -> [GHC.Module]
ambiguousDefinitionModules =
  map head
    . groupModules
    . sortOn renderModuleName
    . map definedIn
  where
    groupModules [] = []
    groupModules (module_ : modules) =
      let (matchingModules, rest) = span ((== renderModuleName module_) . renderModuleName) modules
       in (module_ : matchingModules) : groupModules rest

renderModuleName :: GHC.Module -> Text
renderModuleName =
  T.pack . GHC.moduleNameString . GHC.moduleName

renderExampleQualification :: [AmbiguousQuery] -> Text
renderExampleQualification ambiguousQueries =
  case ambiguousQueries of
    ambiguousQuery : _ ->
      case ambiguousDefinitionModules ambiguousQuery.ambiguousQueryMatches of
        module_ : _ ->
          renderModuleName module_ <> "." <> queryOccName ambiguousQuery.ambiguousQueryText
        [] ->
          ambiguousQuery.ambiguousQueryText
    [] ->
      "<module>.<symbol>"

queryOccName :: Text -> Text
queryOccName queryText =
  case reverse (T.splitOn "." queryText) of
    occName : _ | not (T.null occName) -> occName
    _ -> queryText

pluralSuffix :: Int -> Text
pluralSuffix count
  | count == 1 = ""
  | otherwise = "s"

ambiguousVerb :: Int -> Text
ambiguousVerb count
  | count == 1 = "is"
  | otherwise = "are"

missingSymbolsSection :: [Text] -> [Text]
missingSymbolsSection [] = []
missingSymbolsSection missingSymbols =
  [ T.intercalate "\n" $
      [ T.pack (show (length missingSymbols))
          <> " requested name"
          <> pluralSuffix (length missingSymbols)
          <> " "
          <> missingVerb (length missingSymbols)
          <> " not found:"
      ]
        <> map (("  - " <>) . quoteText) missingSymbols
  ]

missingVerb :: Int -> Text
missingVerb count
  | count == 1 = "was"
  | otherwise = "were"

quoteTexts :: [Text] -> Text
quoteTexts values =
  "[" <> T.intercalate ", " (map (\value -> "\"" <> value <> "\"") values) <> "]"

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

maxRenderedDefinitionResults :: Int
maxRenderedDefinitionResults = 30

maxRenderedOmittedSymbolsPerModule :: Int
maxRenderedOmittedSymbolsPerModule = 10
