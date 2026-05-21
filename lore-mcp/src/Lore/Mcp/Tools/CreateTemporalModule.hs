module Lore.Mcp.Tools.CreateTemporalModule
  ( createTemporalModuleTool,
  )
where

import qualified Data.Text as T
import Lore (MonadLore, createTemporalModule)
import Lore.Mcp.Internal.LoreDoc (LoreDoc, bulletList, heading2, numberedListFrom, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))

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
  path <- createTemporalModule
  pure $
    paragraph ("Temporal module initialized at: " <> T.pack path)
      <> heading2 "Workflow"
      <> numberedListFrom
        1
        [ paragraph "Write custom logic and necessary imports directly into this file.",
          paragraph "Call reloadHomeModules to compile and load it into the session.",
          paragraph "Use executeCode to run your target functions."
        ]
      <> heading2 "Notes"
      <> bulletList
        [ paragraph "Active Haskell extensions set may be different.",
          paragraph "Reuse this file until your debugging task is done.",
          paragraph "Delete it when finished to detach."
        ]
