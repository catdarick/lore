{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lore.Internal.AutoRefactor.ImportRewrite
  ( ImportRewriteResult (..),
    rewriteImportsInFile,
  )
where

import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import Lore.Diagnostics (Span (..))
import Lore.Internal.AutoRefactor.Edit (FileEdit (ReplaceSpanEdit))
import Lore.Internal.AutoRefactor.ImportDecl
  ( NormalizedImport (..),
    ParsedImport (..),
    normalizedImportFromParsed,
    parseImports,
    renderNormalizedImport,
    srcSpanToSpan,
  )
import Lore.Internal.AutoRefactor.ImportNormalize (applyImportOperations)
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation)

data ImportRewriteResult = ImportRewriteResult
  { rewriteEdits :: [FileEdit],
    rewriteLogs :: [String]
  }

rewriteImportsInFile :: FilePath -> GHC.ParsedModule -> Text -> [ImportOperation] -> ImportRewriteResult
rewriteImportsInFile filePath parsedModule source operations =
  let parsedImports = parseImports parsedModule
      normalizedImports = map normalizedImportFromParsed parsedImports
      (rewrittenImports, rewriteLogs) = applyImportOperations normalizedImports operations
      replacementText = renderImportBlock rewrittenImports
   in case importBlockTarget parsedModule source parsedImports of
        Nothing ->
          ImportRewriteResult [] rewriteLogs
        Just targetSpan
          | replacementText == spanText source targetSpan ->
              ImportRewriteResult [] rewriteLogs
          | otherwise ->
              ImportRewriteResult
                { rewriteEdits = [ReplaceSpanEdit filePath targetSpan replacementText],
                  rewriteLogs
                }

renderImportBlock :: [NormalizedImport] -> Text
renderImportBlock [] = ""
renderImportBlock imports =
  T.intercalate "\n" (map renderNormalizedImport (sortOn normalizedImportOrder imports)) <> "\n"

importBlockTarget :: GHC.ParsedModule -> Text -> [ParsedImport] -> Maybe Span
importBlockTarget parsedModule source = \case
  firstImport : restImports ->
    Just $
      Span
        { spanFile = firstImport.parsedImportSpan.spanFile,
          spanStartLine = firstImport.parsedImportSpan.spanStartLine,
          spanStartCol = 1,
          spanEndLine = extendedEnd.spanEndLine,
          spanEndCol = extendedEnd.spanEndCol
        }
    where
      lastImport = last (firstImport : restImports)
      extendedEnd = extendToWholeLineIfPossible source lastImport.parsedImportSpan
  [] ->
    insertionSpan parsedModule source

insertionSpan :: GHC.ParsedModule -> Text -> Maybe Span
insertionSpan parsedModule source = do
  let GHC.L _ GHC.HsModule {GHC.hsmodName, GHC.hsmodExports} = GHC.pm_parsed_source parsedModule
      headerSpan =
        case hsmodExports of
          Just exports -> srcSpanToSpan (GHC.locA (GHC.getLoc exports))
          Nothing ->
            hsmodName >>= (srcSpanToSpan . GHC.locA . GHC.getLoc)
  case headerSpan of
    Just span' ->
      offsetSpan source =<< lineStartAfterWhere source span'
    Nothing ->
      offsetSpan source (findPreambleInsertionOffset source)

lineStartAfterWhere :: Text -> Span -> Maybe Int
lineStartAfterWhere source span' = do
  (_, headerSuffix) <- splitAtSpanEnd source span'
  whereIndex <- findSubstringOffset "where" headerSuffix
  let afterWhere = T.drop (whereIndex + T.length ("where" :: Text)) headerSuffix
  case T.findIndex (== '\n') afterWhere of
    Just newlineIndex ->
      pure (spanEndOffset source span' + whereIndex + T.length ("where" :: Text) + newlineIndex + 1)
    Nothing ->
      pure (T.length source)

findPreambleInsertionOffset :: Text -> Int
findPreambleInsertionOffset source =
  offsetAfterLines source (length (takeWhile isPreambleLine (T.lines source)))
  where
    isPreambleLine line =
      let stripped = T.stripStart line
       in T.null stripped
            || "{-#" `T.isPrefixOf` stripped
            || "--" `T.isPrefixOf` stripped
            || "{-" `T.isPrefixOf` stripped
            || "#!" `T.isPrefixOf` stripped

offsetAfterLines :: Text -> Int -> Int
offsetAfterLines source lineCount =
  T.length $
    T.intercalate "\n" $
      take lineCount (T.lines source)
        <> if lineCount > 0 then [""] else []

offsetSpan :: Text -> Int -> Maybe Span
offsetSpan source offset = do
  (line, col) <- offsetToPosition source offset
  pure
    Span
      { spanFile = "",
        spanStartLine = line,
        spanStartCol = col,
        spanEndLine = line,
        spanEndCol = col
      }

extendToWholeLineIfPossible :: Text -> Span -> Span
extendToWholeLineIfPossible contents span'
  | Just (_, endOffset) <- spanToOffsets contents span',
    let after = dropText endOffset contents,
    let (spaces, rest) = T.span (\ch -> isSpace ch && ch /= '\n') after,
    not (T.null rest),
    T.head rest == '\n',
    Just (newEndLine, newEndCol) <- offsetToPosition contents (endOffset + T.length spaces + 1) =
      span' {spanEndLine = newEndLine, spanEndCol = newEndCol}
  | otherwise = span'

spanText :: Text -> Span -> Text
spanText source span' =
  case spanToOffsets source span' of
    Just (startOffset, endOffset) ->
      T.take (endOffset - startOffset) (T.drop startOffset source)
    Nothing ->
      ""

splitAtSpanEnd :: Text -> Span -> Maybe (Text, Text)
splitAtSpanEnd source span' = do
  (_, endOffset) <- spanToOffsets source span'
  pure (T.take endOffset source, T.drop endOffset source)

spanEndOffset :: Text -> Span -> Int
spanEndOffset source span' =
  maybe 0 snd (spanToOffsets source span')

findSubstringOffset :: Text -> Text -> Maybe Int
findSubstringOffset needle haystack =
  case T.breakOn needle haystack of
    (prefix, suffix)
      | T.null suffix -> Nothing
      | otherwise -> Just (T.length prefix)

spanToOffsets :: Text -> Span -> Maybe (Int, Int)
spanToOffsets contents Span {spanStartLine, spanStartCol, spanEndLine, spanEndCol} = do
  startOffset <- positionToOffset contents (spanStartLine, spanStartCol)
  endOffset <- positionToOffset contents (spanEndLine, spanEndCol)
  pure (startOffset, endOffset)

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

dropText :: Int -> Text -> Text
dropText =
  T.drop
