module Lore.Mcp.Tools.GetDefinition.Shared
  ( CommonGetDefinitionArgs (..),
    GetDefinitionResult,
    GetDefinitionOutput (..),
    GetDefinitionFailed (..),
    GetDefinitionFailure (..),
    GetDefinitionReady (..),
    OmittedDefinitions (..),
    ModuleOmittedSymbols (..),
    FilteredDefinitions (..),
    BuildDefinitionsStrategy,
    defaultRecursionDepth,
    maxRenderedDefinitionResults,
    getDefinitionHandlerWithStrategy,
    buildPaginatedDefinitionSourceFiles,
    mkOmittedDefinitions,
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
    lookupSymbolInfo,
    resolveDefinitionClosureSourcesNamed,
    resolveDefinitionSourceNamed,
  )
import Lore.Definition.RenderSlice (definitionSourceToRenderSlice)
import Lore.Mcp.Internal.LoreDoc
  ( LoreDoc,
    SourceFile,
    ToLoreDoc (toLoreDoc),
    paragraph,
    sourceFile,
  )
import Lore.Mcp.Tools.Shared
  ( Paginated (..),
    PaginationRenderConfig (..),
    PartialLoadWarning (..),
    ToolRun (..),
    loadedSessionPartialWarning,
    paginationSummaryDoc,
    withLoadedSession,
    withPartialLoadWarning,
  )
import Lore.Mcp.Tools.Shared.Source (definitionSlicesToSourceFiles)
import Lore.Mcp.Tools.Shared.SymbolResolution
  ( ResolvedSymbolQuery (resolvedSymbol),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
    unresolvedSymbolQueriesMessage,
  )

data CommonGetDefinitionArgs = CommonGetDefinitionArgs
  { symbols :: [Text],
    skip :: Maybe Int,
    recursionDepth :: Maybe Int
  }

type GetDefinitionResult = ToolRun GetDefinitionOutput

data GetDefinitionOutput
  = GetDefinitionFailedResult GetDefinitionFailed
  | GetDefinitionReadyResult GetDefinitionReady

data GetDefinitionFailed = GetDefinitionFailed
  { getDefinitionFailure :: GetDefinitionFailure,
    getDefinitionFailedPartialLoadWarning :: Maybe PartialLoadWarning
  }

data GetDefinitionFailure
  = GetDefinitionUnresolvedSymbols SymbolsUnresolved
  | GetDefinitionInternalError Text

data GetDefinitionReady = GetDefinitionReady
  { getDefinitionSymbols :: [Text],
    getDefinitionPage :: Maybe (Paginated SourceFile),
    getDefinitionOmitted :: OmittedDefinitions,
    getDefinitionPartialLoadWarning :: Maybe PartialLoadWarning,
    getDefinitionRenderNotifyKnowledgeResetHint :: Bool
  }

data OmittedDefinitions = OmittedDefinitions
  { omittedDefinitionSymbolsByModule :: [ModuleOmittedSymbols],
    omittedDefinitionCount :: Int
  }

data ModuleOmittedSymbols = ModuleOmittedSymbols
  { moduleOmittedSymbolsModuleName :: Text,
    moduleOmittedSymbolsSymbolNames :: [Text]
  }

data FilteredDefinitions = FilteredDefinitions
  { filteredDefinitionPage :: Maybe (Paginated SourceFile),
    filteredOmittedDefinitions :: OmittedDefinitions
  }

type BuildDefinitionsStrategy m =
  Int ->
  [NamedDefinitionSource] ->
  m FilteredDefinitions

getDefinitionHandlerWithStrategy :: (MonadLore m) => Bool -> CommonGetDefinitionArgs -> BuildDefinitionsStrategy m -> m GetDefinitionResult
getDefinitionHandlerWithStrategy shouldRenderNotifyKnowledgeResetHint CommonGetDefinitionArgs {symbols, skip, recursionDepth} buildDefinitions = do
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Definition results may be incomplete."
    eiResolvedQueries <- resolveUniqueSymbolQueries symbols
    case eiResolvedQueries of
      Left unresolvedQueries ->
        pure $
          GetDefinitionFailedResult
            GetDefinitionFailed
              { getDefinitionFailure = GetDefinitionUnresolvedSymbols unresolvedQueries,
                getDefinitionFailedPartialLoadWarning = partialLoadWarning
              }
      Right resolved -> do
        resolvedSymbolInfos <-
          catMaybes
            <$> mapM
              (\resolvedQuery -> lookupSymbolInfo resolvedQuery.resolvedSymbol.name)
              resolved.resolvedQueries
        definitionEntries <- concat <$> mapM (resolveSymbolDefinitions resolvedRecursionDepth) resolvedSymbolInfos
        filteredDefinitions <- buildDefinitions resolvedSkip definitionEntries
        pure $
          GetDefinitionReadyResult
            GetDefinitionReady
              { getDefinitionSymbols = symbols,
                getDefinitionPage = filteredDefinitions.filteredDefinitionPage,
                getDefinitionOmitted = filteredDefinitions.filteredOmittedDefinitions,
                getDefinitionPartialLoadWarning = partialLoadWarning,
                getDefinitionRenderNotifyKnowledgeResetHint = shouldRenderNotifyKnowledgeResetHint
              }
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

buildPaginatedDefinitionSourceFiles ::
  (MonadLore m) =>
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
          visibleSlices <- mapM definitionSourceToSlice paginatedSources.visibleDefinitionSources
          renderedFiles <- liftIO (definitionSlicesToSourceFiles visibleSlices)
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
  where
    definitionSourceToSlice definitionEntry = do
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

mkOmittedDefinitions :: [GHC.Name] -> OmittedDefinitions
mkOmittedDefinitions names =
  OmittedDefinitions
    { omittedDefinitionSymbolsByModule =
        sortOn (.moduleOmittedSymbolsModuleName) (map toModuleOmittedSymbols (Map.toList grouped)),
      omittedDefinitionCount = length names
    }
  where
    grouped =
      foldl' collectDefinition Map.empty names

    collectDefinition groupedByModule name =
      Map.insertWith (<>) (definitionModuleName name) [definitionSymbolName name] groupedByModule

    toModuleOmittedSymbols (moduleName, symbolNames) =
      ModuleOmittedSymbols
        { moduleOmittedSymbolsModuleName = moduleName,
          moduleOmittedSymbolsSymbolNames = dedupeTexts symbolNames
        }

definitionModuleName :: GHC.Name -> Text
definitionModuleName name =
  case GHC.nameModule_maybe name of
    Just module_ -> T.pack (GHC.moduleNameString (GHC.moduleName module_))
    Nothing -> "<unknown module>"

definitionSymbolName :: GHC.Name -> Text
definitionSymbolName =
  T.pack . GHC.getOccString

dedupeTexts :: [Text] -> [Text]
dedupeTexts =
  reverse . snd . foldl' dedupeText (Set.empty, [])
  where
    dedupeText (seenTexts, deduped) value
      | Set.member value seenTexts =
          (seenTexts, deduped)
      | otherwise =
          (Set.insert value seenTexts, value : deduped)

quoteTexts :: [Text] -> Text
quoteTexts values =
  "[" <> T.intercalate ", " (map (\value -> "\"" <> value <> "\"") values) <> "]"

omittedDefinitionsSectionHeader :: Text
omittedDefinitionsSectionHeader =
  "The following definitions are completely UNCHANGED and were omitted from this tool response to optimize token usage. They are already fresh and valid inside your current context window:"

allOmittedDefinitionsSectionHeader :: Text
allOmittedDefinitionsSectionHeader =
  "All matching definitions are completely UNCHANGED and were omitted from this tool response to optimize token usage. They are already fresh and valid inside your current context window:"

notifyKnowledgeResetHint :: Text
notifyKnowledgeResetHint =
  "IF AND ONLY IF your active conversation history was just wiped, or you have suffered a total memory reset and literally cannot see these definitions in your previous turns, you should execute the `notifyKnowledgeReset` tool to resync the server cache."

renderOmittedDefinitionsLines :: OmittedDefinitions -> [Text]
renderOmittedDefinitionsLines omittedDefinitions =
  map ("  - " <>) (map renderModuleLine omittedDefinitions.omittedDefinitionSymbolsByModule)
  where
    renderModuleLine moduleSymbols =
      moduleSymbols.moduleOmittedSymbolsModuleName
        <> ": "
        <> T.intercalate ", " (take maxRenderedOmittedSymbolsPerModule moduleSymbols.moduleOmittedSymbolsSymbolNames)
        <> overflowSuffix moduleSymbols

    overflowSuffix moduleSymbols =
      let hiddenCount = length moduleSymbols.moduleOmittedSymbolsSymbolNames - maxRenderedOmittedSymbolsPerModule
       in if hiddenCount > 0
            then " and " <> T.pack (show hiddenCount) <> " more"
            else ""

instance ToLoreDoc GetDefinitionOutput where
  toLoreDoc = \case
    GetDefinitionFailedResult failed ->
      toLoreDoc failed
    GetDefinitionReadyResult ready ->
      renderReady ready

instance ToLoreDoc GetDefinitionFailed where
  toLoreDoc failed =
    withPartialLoadWarning failed.getDefinitionFailedPartialLoadWarning $
      paragraph (renderGetDefinitionFailure failed.getDefinitionFailure)

instance ToLoreDoc GetDefinitionFailure where
  toLoreDoc =
    paragraph . renderGetDefinitionFailure

renderGetDefinitionFailure :: GetDefinitionFailure -> Text
renderGetDefinitionFailure = \case
  GetDefinitionUnresolvedSymbols unresolvedQueries ->
    unresolvedSymbolQueriesMessage unresolvedQueries
  GetDefinitionInternalError message ->
    message

renderReady :: GetDefinitionReady -> LoreDoc
renderReady ready =
  case ready.getDefinitionPage of
    Nothing
      | ready.getDefinitionOmitted.omittedDefinitionCount > 0 ->
          withPartialLoadWarning ready.getDefinitionPartialLoadWarning $
            paragraph $
              T.intercalate
                "\n"
                ([allOmittedDefinitionsSectionHeader] <> renderOmittedDefinitionsLines ready.getDefinitionOmitted <> notifyHintLines)
      | otherwise ->
          withPartialLoadWarning ready.getDefinitionPartialLoadWarning $
            paragraph ("No definitions found for " <> quoteTexts ready.getDefinitionSymbols <> ".")
    Just page ->
      mconcat
        [ paginationSummaryDoc
            PaginationRenderConfig
              { paginationItemLabel = "definition results",
                paginationSkipArgName = Just "skip"
              }
            page,
          mconcat (map sourceFile page.paginatedItems),
          omittedSection,
          partialWarningSection
        ]
  where
    omittedSection
      | ready.getDefinitionOmitted.omittedDefinitionCount <= 0 =
          mempty
      | otherwise =
          paragraph $
            T.intercalate
              "\n"
              ([omittedDefinitionsSectionHeader] <> renderOmittedDefinitionsLines ready.getDefinitionOmitted <> notifyHintLines)

    notifyHintLines
      | ready.getDefinitionRenderNotifyKnowledgeResetHint =
          [notifyKnowledgeResetHint]
      | otherwise =
          []

    partialWarningSection =
      maybe mempty toLoreDoc ready.getDefinitionPartialLoadWarning

maxRenderedDefinitionResults :: Int
maxRenderedDefinitionResults = 30

maxRenderedOmittedSymbolsPerModule :: Int
maxRenderedOmittedSymbolsPerModule = 10
