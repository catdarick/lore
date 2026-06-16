module Lore.Mcp.Tools.SearchSymbols
  ( searchSymbolsTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, ExampleList, Field, FieldType (..), WithMeta)
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
                        "A short, approximate symbol name — think 'what would this function or type be called?' rather than describing your problem in prose. \
                        \For functions: use action-oriented phrases matching how a developer would name them (e.g. 'savePaymentIntent', 'loadUserProfile'). \
                        \For types and classes: use noun phrases (e.g. 'SessionConfig', 'PaymentMethod'). \
                        \Keep queries concise (2-6 words); long sentences perform poorly because matching is against symbol names, not free text. \
                        \Queries are matched against symbol names, module paths, and type signatures — not against implementations, literals, or documentation. \
                        \Note on ranking: Capitalization guides the results — Uppercase queries bias toward types/classes/constructors; lowercase queries bias toward functions/values."
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
            \Use this ONLY when the symbol name is unknown and needs to be discovered, for example when you are looking for an entry point into some business logic. \
            \When the symbol name is already known, use lookupSymbolInfo for metadata or getDefinition for source directly if you need it, even if the symbol's module name is unknown; \
            \Important: search is performed based on symbol names, module paths, and type signatures — not on implementations, string literals, or documentation bodies. \
            \To search for string literals or patterns inside source files, use a different tool such as rg instead.",
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
