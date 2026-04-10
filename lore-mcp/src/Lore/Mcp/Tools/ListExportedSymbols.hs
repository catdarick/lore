module Lore.Mcp.Tools.ListExportedSymbols
  ( listExportedSymbolsTool,
  )
where

import qualified Data.Aeson as J
import Data.List (intercalate)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified GHC.Plugins as Plugins
import Lore (ExportedSymbolNode (..), LoadTargetsResult (..), MonadLore, SymbolCategory (..), classifySymbolCategory, getLastLoadTargetsResult, listExportedSymbolsByModule)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (appendPartialLoadWarning)

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
        `WithMeta` '[ Description "Optional type occ-name filter. When provided, only exports that mention this type are kept. Useful for narrowing large module export lists. Can be a type, class, or type family name.",
                      Example "Int",
                      Example "Text",
                      Example "Show"
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
        description = Just "List exported symbols for a module visible in the currently loaded session state. Includes direct exports and re-exports. Optionally use typeHint to hide unrelated symbols.",
        handler = listExportedSymbolsHandler
      }

listExportedSymbolsHandler :: (MonadLore m) => ListExportedSymbolsArgs 'ValueType -> m Text
listExportedSymbolsHandler ListExportedSymbolsArgs {moduleName, packageName, typeHint} = do
  maybeLoadResult <- getLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      allSymbols <- listExportedSymbolsByModule moduleName packageName Nothing
      symbols <- listExportedSymbolsByModule moduleName packageName typeHint
      pure (renderExportedSymbolsResult loadResult moduleName packageName typeHint (length allSymbols) symbols)

renderExportedSymbolsResult :: LoadTargetsResult -> Text -> Maybe Text -> Maybe Text -> Int -> [ExportedSymbolNode] -> Text
renderExportedSymbolsResult loadResult moduleName packageName typeHint totalSymbols symbols =
  appendPartialLoadWarning loadResult "Symbol list may be incomplete." renderedBody
  where
    renderedModuleRequest = renderModuleRequest moduleName packageName
    renderedFilterSuffix = renderFilterSuffix typeHint totalSymbols (length symbols)
    renderedCountSuffix = renderCountSuffix typeHint (length symbols)
    renderedBody =
      case symbols of
        [] ->
          "No exported symbols found for module " <> renderedModuleRequest <> renderedFilterSuffix <> "."
        _ ->
          T.unlines $
            [ "Exported symbols in module "
                <> renderedModuleRequest
                <> renderedFilterSuffix
                <> renderedCountSuffix
                <> ":"
            ]
              <> map (("- " <>) . renderSymbolNode) symbols

renderFilterSuffix :: Maybe Text -> Int -> Int -> Text
renderFilterSuffix maybeTypeHint totalSymbols filteredSymbols =
  case maybeTypeHint of
    Nothing ->
      ""
    Just hint ->
      " filtered by type hint "
        <> quoteText hint
        <> " ("
        <> T.pack (show totalSymbols)
        <> " total, "
        <> T.pack (show filteredSymbols)
        <> " kept)"

renderCountSuffix :: Maybe Text -> Int -> Text
renderCountSuffix maybeTypeHint filteredSymbols =
  case maybeTypeHint of
    Nothing ->
      " (" <> T.pack (show filteredSymbols) <> ")"
    Just _ ->
      ""

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

renderModuleRequest :: Text -> Maybe Text -> Text
renderModuleRequest moduleName maybePackageName =
  case maybePackageName of
    Nothing ->
      quoteText moduleName
    Just packageName ->
      quoteText moduleName <> " (package " <> quoteText packageName <> ")"
