module Lore.Tools.Internal.DefinitionLocation
  ( DefinitionLocation (..),
    mkDefinitionLocation,
    renderDefinitionLocationLabel,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as Plugins

newtype DefinitionLocation = DefinitionLocation GHC.RealSrcSpan

renderDefinitionLocationLabel :: DefinitionLocation -> Text
renderDefinitionLocationLabel (DefinitionLocation realSpan) =
  T.pack (Plugins.unpackFS (Plugins.srcSpanFile realSpan))
    <> ":"
    <> T.pack (show (Plugins.srcSpanStartLine realSpan))
    <> ":"
    <> T.pack (show (Plugins.srcSpanStartCol realSpan))
    <> "-"
    <> renderEndPosition realSpan

mkDefinitionLocation :: GHC.Name -> Maybe DefinitionLocation
mkDefinitionLocation name =
  DefinitionLocation <$> Plugins.srcSpanToRealSrcSpan (Plugins.nameSrcSpan name)

renderEndPosition :: GHC.RealSrcSpan -> Text
renderEndPosition realSpan
  | Plugins.srcSpanStartLine realSpan == Plugins.srcSpanEndLine realSpan =
      T.pack (show endColInclusive)
  | otherwise =
      T.pack (show (Plugins.srcSpanEndLine realSpan))
        <> ":"
        <> T.pack (show endColInclusive)
  where
    endColInclusive =
      max (Plugins.srcSpanStartCol realSpan) (Plugins.srcSpanEndCol realSpan - 1)
