module Lore.Mcp.Tools.SearchSymbols
  ( searchSymbolsTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.Result (ResultLimit (..), ToolRun (..))
import Lore.Tools.SearchSymbols
  ( SearchSymbolsOptions (..),
  )
import qualified Lore.Tools.SearchSymbols as ToolsSearchSymbols

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

searchSymbolsHandler :: (MonadLore m) => SearchSymbolsArgs 'ValueType -> m LoreDoc
searchSymbolsHandler SearchSymbolsArgs {query} = do
  result <-
    ToolsSearchSymbols.searchSymbols
      SearchSymbolsOptions
        { searchSymbolsQuery = query,
          searchSymbolsSuggestionLimit =
            Limit (limitToIntWithDefault 10 (symbolSuggestionsLimit mcpDefaultToolPolicy))
        }
  pure $
    case result of
      ToolRunBlocked blocked ->
        toLoreDoc blocked
      ToolRunReady ready ->
        ToolsSearchSymbols.renderSearchSymbolsReady ready
