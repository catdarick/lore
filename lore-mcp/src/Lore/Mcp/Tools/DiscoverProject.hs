module Lore.Mcp.Tools.DiscoverProject
  ( discoverProjectTool,
  )
where

import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))
import Lore.Monad (MonadLore)
import Lore.Tools.DiscoverProject
  ( discoverProject,
    renderDiscoverProject,
  )
import Lore.Tools.Render.Doc (LoreDoc)

discoverProjectTool :: (MonadLore m) => SomeTool m
discoverProjectTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "discoverProject",
        description = Just "Inspect the workspace manifests and return the discovered Haskell packages and their components, including libraries, executables, tests, and benchmarks. Use this to learn package and component names before filtering tests or exploring an unfamiliar workspace. It inspects project configuration and does not require the home modules to compile.",
        handler = discoverProjectHandler
      }

discoverProjectHandler :: (MonadLore m) => m LoreDoc
discoverProjectHandler = do
  output <- discoverProject
  pure (renderDiscoverProject output)
