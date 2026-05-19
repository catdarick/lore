module Lore.Mcp.Tools.SearchSymbols
  ( searchSymbolsTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore, findSimilarSymbols, lookupLastLoadHomeModulesResult, parseAndNormalizeName)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Render (Renderable (..), (|>))
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared.PartialLoadWarning (mkPartialWarning)
import Lore.Mcp.Tools.Shared.SymbolSuggestions
  ( renderSearchSymbolResults,
    symbolSuggestionFetchLimit,
  )

data SearchSymbolsArgs (fieldType :: FieldType) = SearchSymbolsArgs
  { query ::
      Field fieldType Text
        `WithMeta` '[ Description "The text to search for. Can be a specific symbol name (e.g., Some.Module.someFunction) or a natural-language description.",
                      Example "lookupOrZero",
                      Example "Some.Module.someFunction",
                      Example "load picture from database"
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (SearchSymbolsArgs 'ValueType)

instance ToSchema (SearchSymbolsArgs 'MetadataType)

searchSymbolsTool :: (MonadLore m) => SomeTool m
searchSymbolsTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "searchSymbols",
        description =
          Just
            "Fuzzy search for Haskell symbols (functions, types, classes, record selectors etc.) in the current session. Accepts exact names, partial names, or natural-language queries. \
            \Note on ranking: Capitalization of the core symbol name (ignoring module prefixes) guides the results. Uppercase queries bias toward types, classes, and constructors; lowercase queries bias toward functions and values.",
        handler = searchSymbolsHandler
      }

searchSymbolsHandler :: (MonadLore m) => SearchSymbolsArgs 'ValueType -> m Text
searchSymbolsHandler SearchSymbolsArgs {query} = do
  maybeLoadResult <- lookupLastLoadHomeModulesResult
  case maybeLoadResult of
    Nothing ->
      pure "Home modules have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      suggestions <- findSimilarSymbols symbolSuggestionFetchLimit (parseAndNormalizeName query)
      let renderedBody =
            renderSearchSymbolResults query suggestions
          toRender =
            renderedBody
              |> mkPartialWarning loadResult
      pure (renderText toRender)
