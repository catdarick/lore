module Lore.Internal.SourceSpan
  ( srcSpanToSpan,
    realSrcSpanFromSrcSpan,
    spanStartKey,
    spanEndKey,
    spanContains,
    spansOverlap,
    srcSpanSortKey,
    srcSpanSize,
  )
where

import qualified GHC.Data.FastString as FastString
import qualified GHC.Plugins as GHC
import Lore.Diagnostics (Span (..))

srcSpanToSpan :: GHC.SrcSpan -> Maybe Span
srcSpanToSpan = \case
  GHC.RealSrcSpan span' _ ->
    Just
      Span
        { spanFile = FastString.unpackFS (GHC.srcSpanFile span'),
          spanStartLine = GHC.srcSpanStartLine span',
          spanStartCol = GHC.srcSpanStartCol span',
          spanEndLine = GHC.srcSpanEndLine span',
          spanEndCol = GHC.srcSpanEndCol span'
        }
  GHC.UnhelpfulSpan {} ->
    Nothing

realSrcSpanFromSrcSpan :: GHC.SrcSpan -> Maybe GHC.RealSrcSpan
realSrcSpanFromSrcSpan = \case
  GHC.RealSrcSpan realSrcSpan _ ->
    Just realSrcSpan
  GHC.UnhelpfulSpan {} ->
    Nothing

spanStartKey :: Span -> (Int, Int)
spanStartKey Span {spanStartLine, spanStartCol} =
  (spanStartLine, spanStartCol)

spanEndKey :: Span -> (Int, Int)
spanEndKey Span {spanEndLine, spanEndCol} =
  (spanEndLine, spanEndCol)

spanContains :: Span -> Span -> Bool
spanContains outer inner =
  outer.spanFile == inner.spanFile
    && spanStartKey outer <= spanStartKey inner
    && spanEndKey outer >= spanEndKey inner

spansOverlap :: Span -> Span -> Bool
spansOverlap left right =
  left.spanFile == right.spanFile
    && spanStartKey left <= spanEndKey right
    && spanStartKey right <= spanEndKey left

srcSpanSortKey :: GHC.SrcSpan -> (String, Int, Int, Int, Int)
srcSpanSortKey span' =
  case GHC.srcSpanToRealSrcSpan span' of
    Nothing -> ("", maxBound, maxBound, maxBound, maxBound)
    Just realSpan ->
      ( GHC.unpackFS (GHC.srcSpanFile realSpan),
        GHC.srcSpanStartLine realSpan,
        GHC.srcSpanStartCol realSpan,
        GHC.srcSpanEndLine realSpan,
        GHC.srcSpanEndCol realSpan
      )

srcSpanSize :: GHC.SrcSpan -> Int
srcSpanSize span' =
  case GHC.srcSpanToRealSrcSpan span' of
    Nothing ->
      maxBound
    Just realSpan ->
      (GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan) * 10000
        + (GHC.srcSpanEndCol realSpan - GHC.srcSpanStartCol realSpan)
