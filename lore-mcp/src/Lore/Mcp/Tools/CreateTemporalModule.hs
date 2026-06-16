module Lore.Mcp.Tools.CreateTemporalModule
  ( createTemporalModuleTool,
  )
where

import Lore (MonadLore)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))
import Lore.Tools.CreateTemporalModule
  ( createTemporalModule,
    renderCreateTemporalModule,
  )
import Lore.Tools.Render.Doc (LoreDoc)

createTemporalModuleTool :: (MonadLore m) => SomeTool m
createTemporalModuleTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "createTemporalModule",
        description =
          Just
            "Create a temporary Haskell source module attached to the session's home-module load and return its file path. Use this tool in pair with executeCode for debugging or testing. Write multi-line debugging helpers, temporary types, declarations, imports, or instances into that file, then call reloadHomeModules before invoking its definitions. The module remains attached across reloads, is detached if its file is deleted, and is removed from session state when the session restarts.",
        handler = createTemporalModuleHandler
      }

createTemporalModuleHandler :: (MonadLore m) => m LoreDoc
createTemporalModuleHandler = do
  output <- createTemporalModule
  pure (renderCreateTemporalModule output)
