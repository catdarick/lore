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
        description = Just "Scans the workspace for Haskell package manifests (package.yaml or .cabal) to determine project structure. Useful for identifying available packages and their respective components (libraries, targets, executables).",
        handler = discoverProjectHandler
      }

discoverProjectHandler :: (MonadLore m) => m LoreDoc
discoverProjectHandler = do
  output <- discoverProject
  pure (renderDiscoverProject output)
