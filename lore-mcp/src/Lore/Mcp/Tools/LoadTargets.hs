module Lore.Mcp.Tools.LoadTargets where

import Data.Text (Text)
import Lore (LoadTargetsOptions (..), MonadLore, loadTargets)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))

loadTargetsTool :: (MonadLore m) => SomeTool m
loadTargetsTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "loadTargets",
        description = Just "Load the targets of the current project, checking for errors and performing safe auto-fixes if possible.",
        handler = loadTargetsHandler
      }

loadTargetsHandler :: (MonadLore m) => m Text
loadTargetsHandler = do
  loadTargets LoadTargetsOptions {enableAutoRefactor = True}
  pure "ok"
