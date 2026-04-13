module Lore.Mcp.Tools.Shared.Outputable
  ( renderOutputable,
    renderOutputableWith,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Utils.Outputable as Outputable

renderOutputable :: (Outputable.Outputable a) => a -> Text
renderOutputable =
  T.pack . Outputable.showSDocUnsafe . Outputable.ppr

renderOutputableWith :: (a -> Outputable.SDoc) -> a -> Text
renderOutputableWith render =
  T.pack . Outputable.showSDocUnsafe . render
