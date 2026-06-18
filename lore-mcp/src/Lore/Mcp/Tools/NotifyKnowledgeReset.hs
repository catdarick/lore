module Lore.Mcp.Tools.NotifyKnowledgeReset
  ( notifyKnowledgeResetTool,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))
import Lore.Mcp.Monad (MonadLoreMcp, clearSentDefinitionHashes)
import Lore.Tools.Render.Doc (ToLoreDoc (toLoreDoc), paragraph)

notifyKnowledgeResetTool :: (MonadLoreMcp m) => SomeTool m
notifyKnowledgeResetTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "notifyKnowledgeReset",
        description = Just "Notify the server that client-side context was compacted or reset. This clears `getDefinitions` duplicate-suppression memory so previously shown definitions can be returned again.",
        handler = notifyKnowledgeResetHandler
      }

newtype KnowledgeResetResult = KnowledgeResetResult Int

instance ToLoreDoc KnowledgeResetResult where
  toLoreDoc (KnowledgeResetResult clearedHashes) =
    paragraph $
      "Knowledge reset acknowledged. Cleared "
        <> T.pack (show clearedHashes)
        <> " cached definition fingerprint"
        <> pluralSuffix clearedHashes
        <> "."

notifyKnowledgeResetHandler :: (MonadLoreMcp m) => m KnowledgeResetResult
notifyKnowledgeResetHandler = do
  clearedHashes <- clearSentDefinitionHashes
  pure (KnowledgeResetResult clearedHashes)

pluralSuffix :: Int -> Text
pluralSuffix count
  | count == 1 = ""
  | otherwise = "s"
