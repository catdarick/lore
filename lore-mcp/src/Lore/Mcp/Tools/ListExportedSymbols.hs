module Lore.Mcp.Tools.ListExportedSymbols
  ( listExportedSymbolsTool,
  )
where

import qualified Data.Aeson as J
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
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
    lookupLastLoadTargetsResult,
    mkNormalizedModuleName,
    occName,
    parseAndNormalizeName,
    resolveModule,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Render (ListMarker (..), RenderList (..), Renderable (..), Truncation (..), (|>))
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared.PartialLoadWarning (mkPartialWarning)

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

listExportedSymbolsTool :: (MonadLore m) => SomeTool m
listExportedSymbolsTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "listExportedSymbols",
        description = Just "List exported symbols for a module visible in the currently loaded session state. Includes direct exports and re-exports. Optionally use typeHint to keep only exports whose own type/signature structure directly mentions the requested occ-name.",
        handler = listExportedSymbolsHandler
      }

listExportedSymbolsHandler :: (MonadLore m) => ListExportedSymbolsArgs 'ValueType -> m Text
listExportedSymbolsHandler ListExportedSymbolsArgs {moduleName, packageName, typeHint, skip} = do
  maybeLoadResult <- lookupLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      allSymbols <- resolveExportedSymbols moduleName packageName
      let totalSymbols = length allSymbols
          symbolsToRender =
            case typeHint of
              Nothing -> allSymbols
              Just hint ->
                filterExportedSymbolNodesByTypeHint (occName (parseAndNormalizeName hint)) allSymbols
      let toRender =
            renderExportedSymbolsResult resolvedSkip moduleName packageName typeHint totalSymbols symbolsToRender
              |> mkPartialWarning loadResult
      pure (renderText toRender)
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)

resolveExportedSymbols :: (MonadLore m) => Text -> Maybe Text -> m [ExportedSymbolNode]
resolveExportedSymbols moduleName maybePackageName = do
  let normalizedModuleName =
        mkNormalizedModuleName moduleName
  maybeModule <- resolveModule normalizedModuleName maybePackageName
  maybe (pure []) listSymbolsExportedByModule maybeModule

renderExportedSymbolsResult :: Int -> Text -> Maybe Text -> Maybe Text -> Int -> [ExportedSymbolNode] -> Text
renderExportedSymbolsResult skip moduleName packageName typeHint totalSymbols filteredSymbols =
  case NE.nonEmpty filteredSymbols of
    Nothing ->
      allExportsPart <> hintPart <> "."
    Just nonEmptySymbols ->
      renderText (exportedSymbolsList nonEmptySymbols)
  where
    modulePart =
      case packageName of
        Nothing ->
          quoteText moduleName
        Just packageName' ->
          quoteText moduleName <> " (package " <> quoteText packageName' <> ")"
    allExportsPart =
      "Found "
        <> T.pack (show totalSymbols)
        <> " symbols exported from "
        <> modulePart
    hintPart = case typeHint of
      Just hint -> ", " <> T.pack (show (length filteredSymbols)) <> " of which mention type " <> quoteText hint
      Nothing -> ""

    exportedSymbolsList neFilteredSymbols =
      RenderList
        { renderHeader =
            \_ctx ->
              Just $ allExportsPart <> hintPart <> ":",
          contentIndentWidth = 0,
          markerStyle = BulletMarker,
          itemsList = fmap RenderedExportedSymbolNode neFilteredSymbols,
          skip = skip,
          truncation =
            Just
              Truncation
                { maxItems = maxRenderedExportedSymbols,
                  itemName = "exported symbols",
                  skipArgName = Just "skip"
                }
        }

newtype RenderedExportedSymbolNode = RenderedExportedSymbolNode ExportedSymbolNode

instance Renderable RenderedExportedSymbolNode where
  renderText (RenderedExportedSymbolNode node) =
    renderSymbolNode node

renderSymbolNode :: ExportedSymbolNode -> Text
renderSymbolNode node =
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
  T.pack . Plugins.getOccString

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

maxRenderedExportedSymbols :: Int
maxRenderedExportedSymbols = 150
