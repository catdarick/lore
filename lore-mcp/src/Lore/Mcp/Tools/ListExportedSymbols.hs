module Lore.Mcp.Tools.ListExportedSymbols
  ( listExportedSymbolsTool,
  )
where

import qualified Data.Aeson as J
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
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
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.LoreDoc (ToLoreDoc (toLoreDoc), numberedListFrom, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared
  ( Paginated (..),
    PaginationRenderConfig (..),
    PartialLoadWarning,
    ToolRun (..),
    loadedSessionPartialWarning,
    paginateItems,
    paginationSummaryDoc,
    withLoadedSession,
  )
import Lore.Mcp.Tools.Shared.Rendering (quoteText, renderSymbolName)

data ListExportedSymbolsArgs (fieldType :: FieldType) = ListExportedSymbolsArgs
  { moduleName ::
      Field fieldType Text
        `WithMeta` '[ Description "Exact module name to list exported symbols for in the currently loaded session state. The list includes symbols exported directly by the module and symbols it re-exports.",
                      Example "Demo.Support",
                      Example "Data.Text"
                    ],
    packageName ::
      Field fieldType (Maybe Text)
        `WithMeta` '[ Description "Optional package qualifier. Use this only when package modules with the same name need disambiguation.",
                      Example "base"
                    ],
    typeHint ::
      Field fieldType (Maybe Text)
        `WithMeta` '[ Description "Optional type occ-name filter. When provided, only exports whose own type/signature structure directly mentions this type are kept. Useful for narrowing large module export lists. Can be a type, class, or type family name.",
                      Example "Int",
                      Example "Text",
                      Example "Show"
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 5
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (ListExportedSymbolsArgs 'ValueType)

instance ToSchema (ListExportedSymbolsArgs 'MetadataType)

type ListExportedSymbolsResult = ToolRun ListExportedSymbolsReady

data ListExportedSymbolsReady = ListExportedSymbolsReady
  { listExportedSymbolsModuleName :: Text,
    listExportedSymbolsPackageName :: Maybe Text,
    listExportedSymbolsTypeHint :: Maybe Text,
    listExportedSymbolsTotalBeforeHint :: Int,
    listExportedSymbolsPage :: Maybe (Paginated ExportedSymbolNode),
    listExportedSymbolsPartialLoadWarning :: Maybe PartialLoadWarning
  }

instance ToLoreDoc ListExportedSymbolsReady where
  toLoreDoc ready =
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

listExportedSymbolsTool :: (MonadLore m) => SomeTool m
listExportedSymbolsTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "listExportedSymbols",
        description = Just "List exported symbols for a module visible in the currently loaded session state. Includes direct exports and re-exports. Optionally use typeHint to keep only exports whose own type/signature structure directly mentions the requested occ-name.",
        handler = listExportedSymbolsHandler
      }

listExportedSymbolsHandler :: (MonadLore m) => ListExportedSymbolsArgs 'ValueType -> m ListExportedSymbolsResult
listExportedSymbolsHandler ListExportedSymbolsArgs {moduleName, packageName, typeHint, skip} = do
  withLoadedSession \session -> do
    allSymbols <- resolveExportedSymbols moduleName packageName
    let totalSymbols = length allSymbols
        symbolsToRender =
          case typeHint of
            Nothing -> allSymbols
            Just hint ->
              filterExportedSymbolNodesByTypeHint (occName (parseAndNormalizeName hint)) allSymbols
    pure
      ListExportedSymbolsReady
        { listExportedSymbolsModuleName = moduleName,
          listExportedSymbolsPackageName = packageName,
          listExportedSymbolsTypeHint = typeHint,
          listExportedSymbolsTotalBeforeHint = totalSymbols,
          listExportedSymbolsPage = paginateExportedSymbols resolvedSkip symbolsToRender,
          listExportedSymbolsPartialLoadWarning = loadedSessionPartialWarning session "Module export results may be incomplete."
        }
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)

resolveExportedSymbols :: (MonadLore m) => Text -> Maybe Text -> m [ExportedSymbolNode]
resolveExportedSymbols moduleName maybePackageName = do
  let normalizedModuleName =
        mkNormalizedModuleName moduleName
  maybeModule <- resolveModule normalizedModuleName maybePackageName
  maybe (pure []) listSymbolsExportedByModule maybeModule

paginateExportedSymbols :: Int -> [ExportedSymbolNode] -> Maybe (Paginated ExportedSymbolNode)
paginateExportedSymbols skip =
  paginateItems skip maxRenderedExportedSymbols

allExportsPart :: ListExportedSymbolsReady -> Text
allExportsPart ready =
  "Found "
    <> T.pack (show ready.listExportedSymbolsTotalBeforeHint)
    <> " symbols exported from "
    <> modulePart ready

hintPart :: ListExportedSymbolsReady -> Text
hintPart ready =
  case ready.listExportedSymbolsTypeHint of
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
  case ready.listExportedSymbolsPackageName of
    Nothing ->
      quoteText ready.listExportedSymbolsModuleName
    Just packageName ->
      quoteText ready.listExportedSymbolsModuleName <> " (package " <> quoteText packageName <> ")"

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

maxRenderedExportedSymbols :: Int
maxRenderedExportedSymbols = 150
