module Lore.Mcp.Tools.ListExportedSymbols
  ( listExportedSymbolsTool,
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
import Lore.Tools.ListExportedSymbols
  ( ListExportedSymbolsOptions (..),
  )
import qualified Lore.Tools.ListExportedSymbols as ToolsListExportedSymbols
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result
  ( PageRequest (..),
    ResultLimit (..),
  )

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

listExportedSymbolsHandler :: (MonadLore m) => ListExportedSymbolsArgs 'ValueType -> m LoreDoc
listExportedSymbolsHandler ListExportedSymbolsArgs {moduleName, packageName, typeHint, skip} = do
  result <-
    ToolsListExportedSymbols.listExportedSymbols
      ListExportedSymbolsOptions
        { listExportedSymbolsModuleName = moduleName,
          listExportedSymbolsPackageName = packageName,
          listExportedSymbolsTypeHint = typeHint,
          listExportedSymbolsPageRequest =
            PageRequest
              { pageOffset = max 0 (fromMaybe 0 skip),
                pageLimit = Limit (limitToIntWithDefault 150 (exportedSymbolsLimit mcpDefaultToolPolicy))
              }
        }
  pure $ renderToolRun ToolsListExportedSymbols.renderListExportedSymbolsReady result
