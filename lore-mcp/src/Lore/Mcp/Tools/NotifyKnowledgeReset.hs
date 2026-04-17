module Lore.Mcp.Tools.NotifyKnowledgeReset
  ( notifyKnowledgeResetTool,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))
import Lore.Mcp.Monad (MonadLoreMcp, clearSentDefinitionHashes)

notifyKnowledgeResetTool :: (MonadLoreMcp m) => SomeTool m
notifyKnowledgeResetTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "notifyKnowledgeReset",
        description = Just "Notify the server that client-side context was compacted or reset. This clears getDefinition duplicate-suppression memory so previously shown definitions can be returned again.",
        handler = notifyKnowledgeResetHandler
      }

notifyKnowledgeResetHandler :: (MonadLoreMcp m) => m Text
notifyKnowledgeResetHandler = do
  clearedHashes <- clearSentDefinitionHashes
  pure $
    "Knowledge reset acknowledged. Cleared "
      <> T.pack (show clearedHashes)
      <> " cached definition fingerprint"
      <> pluralSuffix clearedHashes
      <> "."

pluralSuffix :: Int -> Text
pluralSuffix count
  | count == 1 = ""
  | otherwise = "s"
