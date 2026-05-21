module Lore.Mcp.Tools.SearchSymbols
  ( searchSymbolsTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore (MonadLore, findSimilarSymbols, parseAndNormalizeName)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.LoreDoc (ToLoreDoc (toLoreDoc), numberedListFrom, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (PartialLoadWarning, ToolRun, loadedSessionPartialWarning, withLoadedSession)
import Lore.Mcp.Tools.Shared.SymbolSuggestions
  ( GroupedSymbolSuggestion,
    groupSymbolSuggestions,
    groupedSymbolSuggestionLabel,
    maxSearchSymbolSuggestions,
    noSymbolsFound,
    quoteText,
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

type SearchSymbolsResult = ToolRun SearchSymbolsReady

data SearchSymbolsReady = SearchSymbolsReady
  { searchSymbolsQuery :: Text,
    searchSymbolsSuggestions :: [GroupedSymbolSuggestion],
    searchSymbolsPartialLoadWarning :: Maybe PartialLoadWarning
  }

instance ToLoreDoc SearchSymbolsReady where
  toLoreDoc ready =
    case ready.searchSymbolsSuggestions of
      [] ->
        mconcat
          [ paragraph (noSymbolsFound ready.searchSymbolsQuery),
            maybe mempty toLoreDoc ready.searchSymbolsPartialLoadWarning
          ]
      suggestions ->
        mconcat
          [ paragraph ("Found " <> T.pack (show (length suggestions)) <> " similar symbols for " <> quoteText ready.searchSymbolsQuery <> ":"),
            numberedListFrom 1 (map (paragraph . groupedSymbolSuggestionLabel) suggestions),
            maybe mempty toLoreDoc ready.searchSymbolsPartialLoadWarning
          ]

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

searchSymbolsHandler :: (MonadLore m) => SearchSymbolsArgs 'ValueType -> m SearchSymbolsResult
searchSymbolsHandler SearchSymbolsArgs {query} = do
  withLoadedSession \session -> do
    suggestions <- findSimilarSymbols symbolSuggestionFetchLimit (parseAndNormalizeName query)
    pure
      SearchSymbolsReady
        { searchSymbolsQuery = query,
          searchSymbolsSuggestions = take maxSearchSymbolSuggestions (groupSymbolSuggestions suggestions),
          searchSymbolsPartialLoadWarning = loadedSessionPartialWarning session "Search results may be incomplete."
        }
