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
        description = Just "Create a new temporary Haskell module in the session temp directory, persist it in session state, and return the created file path. Temporal modules are added to load targets. Automatically pruned on the system reload.",
        handler = createTemporalModuleHandler
      }

createTemporalModuleHandler :: (MonadLore m) => m Text
createTemporalModuleHandler = do
  path <- createTemporalModule
  pure (T.pack path)
