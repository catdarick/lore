module Lore.Mcp.Tools.Shared.CompactClassInstance
  ( CompactClassInstance (..),
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import Lore.Mcp.Internal.Render (Renderable (renderText))
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable)

newtype CompactClassInstance = CompactClassInstance GHC.ClsInst

instance Renderable CompactClassInstance where
  renderText (CompactClassInstance classInstance) =
    stripInstancePrefix (compactRenderedInstanceText (renderOutputable classInstance))

compactRenderedInstanceText :: Text -> Text
compactRenderedInstanceText =
  T.unwords
    . takeWhile (not . isDefinitionCommentLine)
    . filter (not . T.null)
    . map (stripTrailingComment . T.strip)
    . T.lines

stripInstancePrefix :: Text -> Text
stripInstancePrefix text =
  fromMaybe text (T.stripPrefix "instance " text)

stripTrailingComment :: Text -> Text
stripTrailingComment text =
  T.strip $
    case T.breakOn " -- " text of
      (prefix, suffix)
        | T.null suffix -> text
        | otherwise -> prefix

isDefinitionCommentLine :: Text -> Bool
isDefinitionCommentLine =
  T.isPrefixOf "-- Defined"
