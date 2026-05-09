module Lore.Mcp.Internal.SourceSpan
  ( realSrcSpanFromSrcSpan,
  )
where

import qualified GHC.Plugins as GHC

realSrcSpanFromSrcSpan :: GHC.SrcSpan -> Maybe GHC.RealSrcSpan
realSrcSpanFromSrcSpan = \case
  GHC.RealSrcSpan realSrcSpan _ ->
    Just realSrcSpan
  GHC.UnhelpfulSpan {} ->
    Nothing
