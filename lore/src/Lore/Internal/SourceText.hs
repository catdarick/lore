module Lore.Internal.SourceText
  ( positionToOffset,
    offsetToPosition,
    spanToOffsets,
    splitAtSpanEnd,
    spanTextMaybe,
    spanText,
    readSpanText,
    readSpanLines,
    sliceRealSpan,
    relativeSourcePath,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Plugins as GHC
import Lore.Internal.SourceSpan (realSrcSpanFromSrcSpan)
import Lore.Internal.SourceSpan.Types (Span (..))
import System.FilePath (isRelative, makeRelative, normalise)

positionToOffset :: Text -> (Int, Int) -> Maybe Int
positionToOffset contents (targetLine, targetCol)
  | targetLine < 1 || targetCol < 1 = Nothing
  | otherwise = go 1 1 0 (T.unpack contents)
  where
    go line col offset remaining
      | (line, col) == (targetLine, targetCol) = Just offset
      | otherwise =
          case remaining of
            [] -> Nothing
            '\n' : rest -> go (line + 1) 1 (offset + 1) rest
            _ : rest -> go line (col + 1) (offset + 1) rest

offsetToPosition :: Text -> Int -> Maybe (Int, Int)
offsetToPosition contents targetOffset
  | targetOffset < 0 = Nothing
  | otherwise = go 1 1 0 (T.unpack contents)
  where
    go line col offset remaining
      | offset == targetOffset = Just (line, col)
      | otherwise =
          case remaining of
            [] ->
              if offset == targetOffset
                then Just (line, col)
                else Nothing
            '\n' : rest -> go (line + 1) 1 (offset + 1) rest
            _ : rest -> go line (col + 1) (offset + 1) rest

spanToOffsets :: Text -> Span -> Maybe (Int, Int)
spanToOffsets contents Span {spanStartLine, spanStartCol, spanEndLine, spanEndCol} = do
  startOffset <- positionToOffset contents (spanStartLine, spanStartCol)
  endOffset <- positionToOffset contents (spanEndLine, spanEndCol)
  pure (startOffset, endOffset)

splitAtSpanEnd :: Text -> Span -> Maybe (Text, Text)
splitAtSpanEnd source span' = do
  (_, endOffset) <- spanToOffsets source span'
  pure (T.take endOffset source, T.drop endOffset source)

spanTextMaybe :: Text -> Span -> Maybe Text
spanTextMaybe source span' = do
  (startOffset, endOffset) <- spanToOffsets source span'
  pure (T.take (endOffset - startOffset) (T.drop startOffset source))

spanText :: Text -> Span -> Text
spanText source span' =
  maybe "" id (spanTextMaybe source span')

readSpanText :: GHC.SrcSpan -> IO Text
readSpanText span' =
  T.intercalate "\n" <$> readSpanLines span'

readSpanLines :: GHC.SrcSpan -> IO [Text]
readSpanLines span' =
  case realSrcSpanFromSrcSpan span' of
    Nothing ->
      pure ["<definition source unavailable>"]
    Just realSpan ->
      sliceRealSpan realSpan . T.lines <$> TIO.readFile (GHC.unpackFS (GHC.srcSpanFile realSpan))

sliceRealSpan :: GHC.RealSrcSpan -> [Text] -> [Text]
sliceRealSpan realSpan fileLines =
  case drop (GHC.srcSpanStartLine realSpan - 1) fileLines of
    [] ->
      []
    relevantLines ->
      zipWith
        sliceLine
        [GHC.srcSpanStartLine realSpan .. GHC.srcSpanEndLine realSpan]
        (take (GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan + 1) relevantLines)
  where
    sliceLine lineNo line
      | lineNo == GHC.srcSpanStartLine realSpan && lineNo == GHC.srcSpanEndLine realSpan =
          T.take width (T.drop startCol line)
      | lineNo == GHC.srcSpanStartLine realSpan =
          T.drop startCol line
      | lineNo == GHC.srcSpanEndLine realSpan =
          T.take endCol line
      | otherwise =
          line
      where
        startCol = GHC.srcSpanStartCol realSpan - 1
        endCol = GHC.srcSpanEndCol realSpan - 1
        width = endCol - startCol

relativeSourcePath :: FilePath -> FilePath -> FilePath
relativeSourcePath currentDirectory sourcePath =
  normalise $
    if isRelative sourcePath
      then sourcePath
      else makeRelative currentDirectory sourcePath
