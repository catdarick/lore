module Lore.Tools.ListExportedSymbols
  ( ListExportedSymbolsOptions (..),
    ListExportedSymbolsResult,
    ListExportedSymbolsReady (..),
    listExportedSymbols,
    renderListExportedSymbolsReady,
  )
where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as Plugins
import Lore
  ( ExportedSymbolNode (..),
    MonadLore,
    SymbolCategory (..),
    classifySymbolCategory,
    filterExportedSymbolNodesByTypeHint,
    listSymbolsExportedByModule,
    mkNormalizedModuleName,
    occName,
    parseAndNormalizeName,
    resolveModule,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc), numberedListFrom, paragraph)
import Lore.Tools.Render.Text (quoteText, renderSymbolName)
import Lore.Tools.Result
  ( Paginated (..),
    PaginationRenderConfig (..),
    PageRequest (..),
    PartialLoadWarning,
    ToolRun,
    loadedSessionPartialWarning,
    paginateItemsWithPageRequest,
    paginationSummaryDoc,
    withLoadedSession,
  )

data ListExportedSymbolsOptions = ListExportedSymbolsOptions
  { listExportedSymbolsModuleName :: Text,
    listExportedSymbolsPackageName :: Maybe Text,
    listExportedSymbolsTypeHint :: Maybe Text,
    listExportedSymbolsPageRequest :: PageRequest
  }
  deriving stock (Eq, Show)

type ListExportedSymbolsResult = ToolRun ListExportedSymbolsReady

data ListExportedSymbolsReady = ListExportedSymbolsReady
  { listExportedSymbolsReadyModuleName :: Text,
    listExportedSymbolsReadyPackageName :: Maybe Text,
    listExportedSymbolsReadyTypeHint :: Maybe Text,
    listExportedSymbolsTotalBeforeHint :: Int,
    listExportedSymbolsPage :: Maybe (Paginated ExportedSymbolNode),
    listExportedSymbolsPartialLoadWarning :: Maybe PartialLoadWarning
  }

listExportedSymbols :: (MonadLore m) => ListExportedSymbolsOptions -> m ListExportedSymbolsResult
listExportedSymbols options = do
  withLoadedSession \session -> do
    allSymbols <- resolveExportedSymbols options.listExportedSymbolsModuleName options.listExportedSymbolsPackageName
    let totalSymbols = length allSymbols
        symbolsToRender =
          case options.listExportedSymbolsTypeHint of
            Nothing -> allSymbols
            Just hint ->
              filterExportedSymbolNodesByTypeHint (occName (parseAndNormalizeName hint)) allSymbols
    pure
      ListExportedSymbolsReady
        { listExportedSymbolsReadyModuleName = options.listExportedSymbolsModuleName,
          listExportedSymbolsReadyPackageName = options.listExportedSymbolsPackageName,
          listExportedSymbolsReadyTypeHint = options.listExportedSymbolsTypeHint,
          listExportedSymbolsTotalBeforeHint = totalSymbols,
          listExportedSymbolsPage = paginateExportedSymbols options.listExportedSymbolsPageRequest symbolsToRender,
          listExportedSymbolsPartialLoadWarning = loadedSessionPartialWarning session "Module export results may be incomplete."
        }

renderListExportedSymbolsReady :: ListExportedSymbolsReady -> LoreDoc
renderListExportedSymbolsReady ready =
  case ready.listExportedSymbolsPage of
    Nothing ->
      mconcat
        [ paragraph (allExportsPart ready <> hintPart ready <> "."),
          maybe mempty toLoreDoc ready.listExportedSymbolsPartialLoadWarning
        ]
    Just page ->
      mconcat
        [ paragraph (allExportsPart ready <> hintPart ready <> ":"),
          paginationSummaryDoc
            PaginationRenderConfig
              { paginationItemLabel = "exported symbols",
                paginationSkipArgName = Just "skip"
              }
            page,
          numberedListFrom (fromIntegral (page.paginatedSkippedItems + 1)) (map (paragraph . renderExportedSymbolNodeLabel) page.paginatedItems),
          maybe mempty toLoreDoc ready.listExportedSymbolsPartialLoadWarning
        ]

resolveExportedSymbols :: (MonadLore m) => Text -> Maybe Text -> m [ExportedSymbolNode]
resolveExportedSymbols moduleName maybePackageName = do
  let normalizedModuleName =
        mkNormalizedModuleName moduleName
  maybeModule <- resolveModule normalizedModuleName maybePackageName
  maybe (pure []) listSymbolsExportedByModule maybeModule

paginateExportedSymbols :: PageRequest -> [ExportedSymbolNode] -> Maybe (Paginated ExportedSymbolNode)
paginateExportedSymbols pageRequest =
  paginateItemsWithPageRequest pageRequest

allExportsPart :: ListExportedSymbolsReady -> Text
allExportsPart ready =
  "Found "
    <> T.pack (show ready.listExportedSymbolsTotalBeforeHint)
    <> " symbols exported from "
    <> modulePart ready

hintPart :: ListExportedSymbolsReady -> Text
hintPart ready =
  case ready.listExportedSymbolsReadyTypeHint of
    Just hint ->
      ", "
        <> T.pack (show renderedCount)
        <> " of which mention type "
        <> quoteText hint
    Nothing ->
      ""
  where
    renderedCount =
      maybe 0 (.paginatedTotalItems) ready.listExportedSymbolsPage

modulePart :: ListExportedSymbolsReady -> Text
modulePart ready =
  case ready.listExportedSymbolsReadyPackageName of
    Nothing ->
      quoteText ready.listExportedSymbolsReadyModuleName
    Just packageName ->
      quoteText ready.listExportedSymbolsReadyModuleName <> " (package " <> quoteText packageName <> ")"

renderExportedSymbolNodeLabel :: ExportedSymbolNode -> Text
renderExportedSymbolNodeLabel node =
  baseLabel
    <> case node.nodeChildren of
      [] -> ""
      childNodes ->
        " (" <> T.pack (intercalate ", " (map (T.unpack . renderOccName . (.nodeName)) childNodes)) <> ")"
  where
    baseLabel =
      case categoryLabel (classifySymbolCategory node.nodeThing) of
        Nothing -> renderOccName node.nodeName
        Just label -> label <> " " <> renderOccName node.nodeName

categoryLabel :: SymbolCategory -> Maybe Text
categoryLabel = \case
  SymbolClass -> Just "class"
  SymbolData -> Just "data"
  SymbolNewtype -> Just "newtype"
  SymbolTypeAlias -> Just "type"
  SymbolTypeFamily -> Just "type family"
  SymbolDataFamily -> Just "data family"
  _ -> Nothing

renderOccName :: Plugins.Name -> Text
renderOccName =
  renderSymbolName
