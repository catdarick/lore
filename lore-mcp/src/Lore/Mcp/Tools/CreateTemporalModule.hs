module Lore.Mcp.Tools.CreateTemporalModule
  ( createTemporalModuleTool,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore (MonadLore, createTemporalModule)
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

createTemporalModuleHandler :: (MonadLore m) => m Text
createTemporalModuleHandler = do
  path <- createTemporalModule
  pure $
    T.unlines
      [ "Temporal module initialized at: " <> T.pack path,
        "",
        "Workflow:",
        "  1. Write custom logic and necessary imports directly into this file.",
        "  2. Call 'reloadHomeModules' to compile and load it into the session.",
        "  3. Use 'executeCode' to run your target functions.",
        "",
        "Note: Active Haskell extensions set may be different. Reuse this file until your debugging task is done. Delete it when finished to detach."
      ]
