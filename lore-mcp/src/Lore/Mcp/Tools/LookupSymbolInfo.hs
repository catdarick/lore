module Lore.Mcp.Tools.LookupSymbolInfo
  ( lookupSymbolInfoTool,
  )
where

import qualified Data.Aeson as J
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore (MonadLore, Symbol (..), SymbolInfo (..), SymbolSuggestion (..), findMatchingSymbols, findSimilarSymbols, listDirectInstances, lookupLastLoadTargetsResult, lookupSymbolInfo, parseAndNormalizeName)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Render
  ( ListMarker (..),
    RenderList (..),
    Renderable (..),
    Truncation (..),
    totalItems,
    (|>),
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared.DetailedSymbolInfo (DetailedSymbolInfo (..))
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable)
import Lore.Mcp.Tools.Shared.PartialLoadWarning (mkPartialWarning)

data LookupSymbolInfoArgs (fieldType :: FieldType) = LookupSymbolInfoArgs
  { symbol ::
      Field fieldType Text
        `WithMeta` '[ Description "Symbol name to look up. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      Example "lookupOrZero",
                      Example "Some.Module.someFunction"
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 5
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (LookupSymbolInfoArgs 'ValueType)

instance ToSchema (LookupSymbolInfoArgs 'MetadataType)

lookupSymbolInfoTool :: (MonadLore m) => SomeTool m
lookupSymbolInfoTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "lookupSymbolInfo",
        description =
          Just
            "Look up metadata and information for a Haskell symbol in the current session. \
            \Supports module-qualified queries and semantic fuzzy matching. \
            \Note on scope: Unexported top-level symbols are available for home modules. For package modules, exported symbols remain visible even if a home-module load fails (provided a load was attempted). \
            \During partial loads, 'No symbols found' only means the symbol isn't in the loaded session; it does not prove it is absent from the source.",
        handler = lookupSymbolInfoHandler
      }

lookupSymbolInfoHandler :: (MonadLore m) => LookupSymbolInfoArgs 'ValueType -> m Text
lookupSymbolInfoHandler LookupSymbolInfoArgs {symbol, skip} = do
  maybeLoadResult <- lookupLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      symbolInfos <- lookupExactSymbolInfos symbol
      renderedBody <-
        case NE.nonEmpty symbolInfos of
          Nothing -> do
            suggestions <- findSimilarSymbols maxRenderedSymbolSuggestions (parseAndNormalizeName symbol)
            pure (renderMissingSymbol symbol suggestions)
          Just nonEmptySymbolInfos -> do
            detailedSymbolInfos <- mapM mkDetailedSymbolInfo nonEmptySymbolInfos
            pure (renderResolvedSymbols detailedSymbolInfos)
      let toRender =
            renderedBody
              |> mkPartialWarning loadResult
      pure $ renderText toRender
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)

    renderResolvedSymbols detailedSymbolInfos =
      renderText (renderSymbolCandidatesList resolvedSkip detailedSymbolInfos)

maxRenderedSymbolSuggestions :: Int
maxRenderedSymbolSuggestions = 10

renderSymbolCandidatesList :: Int -> NonEmpty DetailedSymbolInfo -> RenderList
renderSymbolCandidatesList skip detailedSymbolInfos =
  RenderList
    { renderHeader =
        \ctx -> Just $ "Found " <> T.pack (show ctx.totalItems) <> " symbol candidates:",
      contentIndentWidth = 0,
      markerStyle = NumberMarker,
      itemsList = detailedSymbolInfos,
      skip = skip,
      truncation =
        Just
          Truncation
            { maxItems = maxRenderedSymbolCandidates,
              itemName = "symbol candidates",
              skipArgName = Just "skip"
            }
    }
  where
    maxRenderedSymbolCandidates = 5

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

newtype RenderedSymbolSuggestion = RenderedSymbolSuggestion SymbolSuggestion

instance Renderable RenderedSymbolSuggestion where
  renderText (RenderedSymbolSuggestion suggestion) =
    let lookupName = suggestion.suggestedLookupName
        symbolName = renderOutputable suggestion.suggestedSymbol.name
     in if lookupName == symbolName
          then symbolName
          else lookupName <> " (" <> symbolName <> ")"

renderMissingSymbol :: Text -> [SymbolSuggestion] -> Text
renderMissingSymbol query suggestions =
  case NE.nonEmpty (map RenderedSymbolSuggestion suggestions) of
    Nothing ->
      noSymbolsFound
    Just nonEmptySuggestions ->
      renderText $
        RenderList
          { renderHeader = \_ -> Just $ noSymbolsFound <> " Maybe you meant one of these?",
            contentIndentWidth = 0,
            markerStyle = NumberMarker,
            itemsList = nonEmptySuggestions,
            skip = 0,
            truncation = Nothing
          }
  where
    noSymbolsFound =
      "No symbols found for " <> quoteText query <> "."

lookupExactSymbolInfos :: (MonadLore m) => Text -> m [SymbolInfo]
lookupExactSymbolInfos query = do
  matchedSymbols <- Set.toList <$> findMatchingSymbols (parseAndNormalizeName query)
  catMaybes <$> mapM (lookupSymbolInfo . (.name)) matchedSymbols

mkDetailedSymbolInfo :: (MonadLore m) => SymbolInfo -> m DetailedSymbolInfo
mkDetailedSymbolInfo symbolInfo = do
  instancesInfo <- listDirectInstances (symbolName symbolInfo)
  pure
    DetailedSymbolInfo
      { symbolInfo,
        instancesInfo
      }
