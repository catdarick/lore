{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.ImportCleanup.SourceSlice
  ( SourceSlice (..),
    sliceRange,
    spanToRange,
    rangeToSpan,
    replaceRange,
    findFirstBalancedParensRange,
    findBalancedParensRangeFrom,
    lineStartOffsetAt,
    lineEndOffsetFrom,
    includeTrailingNewline,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.ImportCleanup.Types (SourceRange (..))
import Lore.Internal.SourceSpan.Types (Span (..))
import Lore.Internal.SourceText (offsetToPosition, positionToOffset, spanToOffsets)

data SourceSlice = SourceSlice
  { sourceSliceRange :: SourceRange,
    sourceSliceText :: Text
  }
  deriving (Eq, Show)

sliceRange :: Text -> SourceRange -> Maybe SourceSlice
sliceRange source range@SourceRange {rangeStart, rangeEnd}
  | rangeStart < 0 =
      Nothing
  | rangeEnd < rangeStart =
      Nothing
  | rangeEnd > T.length source =
      Nothing
  | otherwise =
      Just
        SourceSlice
          { sourceSliceRange = range,
            sourceSliceText = T.take (rangeEnd - rangeStart) (T.drop rangeStart source)
          }

spanToRange :: Text -> Span -> Maybe SourceRange
spanToRange source span' = do
  (startOffset, endOffset) <- spanToOffsets source span'
  pure
    SourceRange
      { rangeStart = startOffset,
        rangeEnd = endOffset
      }

rangeToSpan :: Text -> FilePath -> SourceRange -> Maybe Span
rangeToSpan source spanFile SourceRange {rangeStart, rangeEnd} = do
  (startLine, startCol) <- offsetToPosition source rangeStart
  (endLine, endCol) <- offsetToPosition source rangeEnd
  pure
    Span
      { spanFile,
        spanStartLine = startLine,
        spanStartCol = startCol,
        spanEndLine = endLine,
        spanEndCol = endCol
      }

replaceRange :: Text -> SourceRange -> Text -> Maybe Text
replaceRange source range replacement = do
  _ <- sliceRange source range
  pure (T.take range.rangeStart source <> replacement <> T.drop range.rangeEnd source)

findFirstBalancedParensRange :: Text -> Maybe SourceRange
findFirstBalancedParensRange source =
  findBalancedParensRangeFrom source 0

findBalancedParensRangeFrom :: Text -> Int -> Maybe SourceRange
findBalancedParensRangeFrom source startIndex
  | startIndex < 0 =
      Nothing
  | startIndex >= sourceLength =
      Nothing
  | otherwise =
      go startIndex
  where
    sourceLength = T.length source

    go index
      | index >= sourceLength =
          Nothing
      | otherwise =
          case T.index source index of
            '(' ->
              fmap
                (\closeIndex -> SourceRange index (closeIndex + 1))
                (findMatchingClose (index + 1) 1)
            _ ->
              go (index + 1)

    findMatchingClose :: Int -> Int -> Maybe Int
    findMatchingClose offset depth
      | offset >= sourceLength =
          Nothing
      | otherwise =
          case T.index source offset of
            '(' ->
              findMatchingClose (offset + 1) (depth + 1)
            ')' ->
              if depth == 1
                then Just offset
                else findMatchingClose (offset + 1) (depth - 1)
            _ ->
              findMatchingClose (offset + 1) depth

lineStartOffsetAt :: Text -> Int -> Maybe Int
lineStartOffsetAt source lineNo =
  positionToOffset source (lineNo, 1)

lineEndOffsetFrom :: Text -> Int -> Int
lineEndOffsetFrom source offset =
  let after = T.drop offset source
   in offset
        + case T.findIndex (== '\n') after of
          Nothing -> T.length after
          Just newlineIndex -> newlineIndex

includeTrailingNewline :: Text -> Int -> Int
includeTrailingNewline source offset =
  case T.uncons (T.drop offset source) of
    Just ('\n', _) -> offset + 1
    Just ('\r', rest)
      | T.isPrefixOf "\n" rest -> offset + 2
      | otherwise -> offset + 1
    _ -> offset
