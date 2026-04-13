module Lore.Mcp.Tools.LookupSymbolInfo
  ( lookupSymbolInfoTool,
  )
where

import qualified Data.Aeson as J
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore (MonadLore, SymbolInfo, getLastLoadTargetsResult, lookupRootSymbolInfo)
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
import Lore.Mcp.Tools.Shared.PartialLoadWarning (mkPartialWarning)

data LookupSymbolInfoArgs (fieldType :: FieldType) = LookupSymbolInfoArgs
  { symbol ::
      Field fieldType Text
        `WithMeta` '[ Description "Exact symbol name to look up in the loaded project symbol table. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
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
        description = Just "Look up information about a symbol visible in the current session state. For home modules, unexported top-level symbols may also be available. For visible package modules, exported symbols can still be resolved after a failed home-module load, as long as a load was attempted. During partial load, 'No symbols found' only means no loaded match was available in the session; it does not prove the symbol is absent from source.",
        handler = lookupSymbolInfoHandler
      }

lookupSymbolInfoHandler :: (MonadLore m) => LookupSymbolInfoArgs 'ValueType -> m Text
lookupSymbolInfoHandler LookupSymbolInfoArgs {symbol, skip} = do
  maybeLoadResult <- getLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      symbolInfos <- lookupRootSymbolInfo symbol
      let toRender =
            mkRenderedBody symbolInfos
              |> mkPartialWarning loadResult
      pure $ renderText toRender
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)

    mkRenderedBody symbolInfos =
      case NE.nonEmpty symbolInfos of
        Nothing ->
          "No symbols found for " <> quoteText symbol <> "."
        Just nonEmptySymbolInfos ->
          renderText (renderSymbolCandidatesList resolvedSkip nonEmptySymbolInfos)

renderSymbolCandidatesList :: Int -> NonEmpty SymbolInfo -> RenderList
renderSymbolCandidatesList skip symbolInfos =
  RenderList
    { renderHeader =
        \ctx -> Just $ "Found " <> T.pack (show ctx.totalItems) <> " symbol candidates:",
      contentIndentWidth = 0,
      markerStyle = NumberMarker,
      itemsList = fmap DetailedSymbolInfo symbolInfos,
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
