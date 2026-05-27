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
            "Create a temporary Haskell module attached to the session home modules load.\
            \Persists across reloads for active reuse during debugging. Automatically detached if deleted; pruned on session restart.",
        handler = createTemporalModuleHandler
      }

createTemporalModuleHandler :: (MonadLore m) => m LoreDoc
createTemporalModuleHandler = do
  output <- createTemporalModule
  pure (renderCreateTemporalModule output)
