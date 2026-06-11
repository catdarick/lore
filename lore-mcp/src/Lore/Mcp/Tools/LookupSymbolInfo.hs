module Lore.Mcp.Tools.LookupSymbolInfo
  ( lookupSymbolInfoTool,
  )
where

import qualified Data.Aeson as J
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), renderToolRun)
import Lore.Tools.LookupSymbolInfo
  ( LookupSymbolInfoOptions (..),
  )
import qualified Lore.Tools.LookupSymbolInfo as ToolsLookupSymbolInfo
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result
  ( PageRequest (..),
    ResultLimit (..),
  )

data LookupSymbolInfoArgs (fieldType :: FieldType) = LookupSymbolInfoArgs
  { symbol ::
      Field fieldType Text
        `WithMeta` '[ Description "Symbol name to look up. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope. Important: if you are not sure about the exact module, omit the module qualification, do not try to guess. Examples: \"lookupOrZero\", \"Some.Module.someFunction\"."
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

lookupSymbolInfoHandler :: (MonadLore m) => LookupSymbolInfoArgs 'ValueType -> m LoreDoc
lookupSymbolInfoHandler LookupSymbolInfoArgs {symbol, skip} = do
  result <-
    ToolsLookupSymbolInfo.lookupSymbolInfo
      LookupSymbolInfoOptions
        { lookupSymbolInfoQuery = symbol,
          lookupSymbolInfoPageRequest =
            PageRequest
              { pageOffset = max 0 (fromMaybe 0 skip),
                pageLimit = Limit (limitToIntWithDefault 5 (symbolCandidatesLimit mcpDefaultToolPolicy))
              },
          lookupSymbolInfoSuggestionLimit =
            Limit (limitToIntWithDefault 10 (symbolSuggestionsLimit mcpDefaultToolPolicy))
        }
  pure $ renderToolRun ToolsLookupSymbolInfo.renderLookupSymbolInfoReady result
