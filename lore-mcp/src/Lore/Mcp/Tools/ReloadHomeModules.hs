module Lore.Mcp.Tools.ReloadHomeModules where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Lore (HomeModulesLoadSummary (..), LoadHomeModulesResult (..), MonadLore, projectEnvironmentFailureMessage)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), Maximum, Minimum, WithMeta)
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
      Field fieldType (Maybe Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial diagnostics to skip. Use it only if you need more context to fix the initial errors.",
                      Minimum 0,
                      Maximum 9999,
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
        description = Just "Reload all project home modules into the current GHC session, refresh symbol and definition indexes, and return compilation diagnostics. The operation may automatically remove redundant imports. It resets interpreter bindings, so values previously introduced interactively are cleared. Use it after source changes and before interpreter or index-dependent operations when the session may be stale.",
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
  case loadResult of
    LoadHomeModulesCompleted summary ->
      J.object
        [ "tool" J..= ("reloadHomeModules" :: String),
          "status" J..= statusText,
          "loadedModules" J..= summary.homeModulesLoaded,
          "failedModules" J..= summary.homeModulesFailed,
          "totalModules" J..= summary.homeModulesTotal,
          "autofixedModules" J..= summary.homeModulesAutofixed,
          "autofixedFiles" J..= summary.homeModulesAutofixedFiles
        ]
    LoadHomeModulesPreparationFailed failure ->
      J.object
        [ "tool" J..= ("reloadHomeModules" :: String),
          "status" J..= statusText,
          "message" J..= projectEnvironmentFailureMessage failure
        ]
  where
    loadResult = renderedResult.renderedResultValue
    statusText =
      case reloadHomeModulesStatus loadResult of
        ReloadHomeModulesStatusSuccess ->
          ("success" :: String)
        ReloadHomeModulesStatusCompilationFailure ->
          "compilation-failure"
        ReloadHomeModulesStatusEnvironmentFailure ->
          "environment-failure"
        ReloadHomeModulesStatusRestartRequired ->
          "restart-required"
