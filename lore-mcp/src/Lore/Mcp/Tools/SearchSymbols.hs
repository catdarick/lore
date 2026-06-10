module Lore.Mcp.Tools.SearchSymbols
  ( searchSymbolsTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, ExampleList, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), renderToolRun)
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result (ResultLimit (..))
import Lore.Tools.SearchSymbols
  ( SearchSymbolsOptions (..),
  )
import qualified Lore.Tools.SearchSymbols as ToolsSearchSymbols

data SearchSymbolsArgs (fieldType :: FieldType) = SearchSymbolsArgs
  { query ::
      Field fieldType Text
        `WithMeta` '[ Description
                        "A natural-language description or approximate concept name for the symbol you are trying to discover. \
                        \Note on ranking: Capitalization of the core symbol name guides the results. Uppercase queries bias toward types, classes, and constructors; lowercase queries bias toward functions and values. \
                        \Examples: \"load picture from database\", \"createUser\", \"SessionConfig\"."
                    ],
    modulePatterns ::
      Field fieldType (Maybe [Text])
        `WithMeta` '[ Description "Optional module-name patterns. A symbol is included when any associated module matches at least one pattern. '*' matches any sequence of characters. To search across all modules, set to null.",
                      ExampleList '["Some.Module.*", "Some.*.Name.*", "Some.Module.Name"]
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
            "Fuzzy search for Haskell symbols (functions, types, classes, record selectors etc.) in the current session. \
            \Use this ONLY when the symbol name is unknown and needs to be discovered via natural-language or approximate queries. \
            \If you already know the symbol name — even without a module qualifier — use lookupSymbolInfo or getDefinition directly instead; \
            \Important: search is performed based on symbol names, module paths, and type signatures — not on implementations, string literals, or documentation bodies. \
            \To search for string literals or patterns inside source files, use rg or grep instead.",
        handler = searchSymbolsHandler
      }

searchSymbolsHandler :: (MonadLore m) => SearchSymbolsArgs 'ValueType -> m LoreDoc
searchSymbolsHandler SearchSymbolsArgs {query, modulePatterns} = do
  compiledModulePatterns <- compileModulePatterns modulePatterns
  result <-
    ToolsSearchSymbols.searchSymbols
      SearchSymbolsOptions
        { searchSymbolsQuery = query,
          searchSymbolsSuggestionLimit =
            Limit (limitToIntWithDefault 10 (symbolSuggestionsLimit mcpDefaultToolPolicy)),
          searchSymbolsModulePatterns = compiledModulePatterns
        }
  pure $ renderToolRun ToolsSearchSymbols.renderSearchSymbolsOutput result

compileModulePatterns :: (Monad m) => Maybe [Text] -> m [ToolsSearchSymbols.SearchSymbolsModulePattern]
compileModulePatterns maybeRawModulePatterns =
  traverse compileModulePattern (maybe [] id maybeRawModulePatterns)
  where
    compileModulePattern rawPattern =
      case ToolsSearchSymbols.mkSearchSymbolsModulePattern rawPattern of
        Right modulePattern -> pure modulePattern
        Left _ -> error "modulePatterns items must be nonempty strings"
