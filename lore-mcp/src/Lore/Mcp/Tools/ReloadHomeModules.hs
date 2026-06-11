module Lore.Mcp.Tools.ReloadHomeModules where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Lore (LoadHomeModulesResult (..), MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.ReloadHomeModules
  ( ReloadHomeModulesOptions (..),
    ReloadHomeModulesStatus (..),
    reloadHomeModulesStatus,
  )
import qualified Lore.Tools.ReloadHomeModules as ToolsReload
import Lore.Tools.Result (PageRequest (..), RenderedResult (..), ResultLimit (..))

data ReloadHomeModulesArgs (fieldType :: FieldType) = ReloadHomeModulesArgs
  { skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial diagnostics to skip. Use it only if you need more context to fix the initial errors.",
                      Example 5
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (ReloadHomeModulesArgs 'ValueType)

instance ToSchema (ReloadHomeModulesArgs 'MetadataType)

reloadHomeModulesTool :: (MonadLore m) => SomeTool m
reloadHomeModulesTool =
  SomeToolWithArgsStructured
    ToolWithArgs
      { name = "reloadHomeModules",
        description = Just "Reloads all home modules, checks for errors, and applies safe auto-fixes when possible. This reload resets interpreter state (interactive bindings are cleared). Run this before tools that need up-to-date module information.",
        handler = reloadHomeModulesHandler
      }
    reloadHomeModulesStructured

reloadHomeModulesHandler :: (MonadLore m) => ReloadHomeModulesArgs 'ValueType -> m (RenderedResult LoadHomeModulesResult)
reloadHomeModulesHandler ReloadHomeModulesArgs {skip} =
  ToolsReload.reloadHomeModules
    ReloadHomeModulesOptions
      { reloadHomeModulesDiagnosticsPageRequest =
          Just
            PageRequest
              { pageOffset = max 0 (maybe 0 id skip),
                pageLimit = Limit (limitToIntWithDefault 5 (diagnosticsLimit mcpDefaultToolPolicy))
              }
      }

reloadHomeModulesStructured :: ReloadHomeModulesArgs 'ValueType -> RenderedResult LoadHomeModulesResult -> J.Value
reloadHomeModulesStructured _ renderedResult =
  J.object
    [ "tool" J..= ("reloadHomeModules" :: String),
      "status" J..= statusText,
      "loadedModules" J..= loadResult.loadHomeModulesLoaded,
      "failedModules" J..= loadResult.loadHomeModulesFailed,
      "totalModules" J..= loadResult.loadHomeModulesTotal,
      "autofixedModules" J..= loadResult.loadHomeModulesAutofixed,
      "autofixedFiles" J..= loadResult.loadHomeModulesAutofixedFiles
    ]
  where
    loadResult = renderedResult.renderedResultValue
    statusText =
      case reloadHomeModulesStatus loadResult of
        ReloadHomeModulesStatusSuccess ->
          ("success" :: String)
        ReloadHomeModulesStatusCompilationFailure ->
          "compilation-failure"
