module Lore.Mcp.Tools.Shared.Rendering
  ( quoteText,
    renderModuleName,
    renderSymbolName,
    renderList,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as Plugins

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

renderModuleName :: Plugins.Module -> Text
renderModuleName module_ =
  T.pack (Plugins.moduleNameString (Plugins.moduleName module_))

renderSymbolName :: Plugins.Name -> Text
renderSymbolName =
  T.pack . Plugins.getOccString

renderList :: [Text] -> Text
renderList [] = "(none)"
renderList values = T.intercalate ", " values
