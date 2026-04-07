{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Internal.AutoRefact.RedundantImports
  ( suggestRedundantImportEdits,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (find, nubBy, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import qualified GHC.Data.FastString as FastString
import GHC.Hs (GhcPs, HsModule (..), IE (..), IEWrappedName (..), ImportDecl (..), LIE, LImportDecl)
import qualified GHC.Utils.Outputable as Outputable
import Internal.AutoRefact.Edit (FileEdit (..))
import Internal.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..))
import Monad (MonadLore)
import System.FilePath (normalise)

suggestRedundantImportEdits :: (MonadLore m) => Map.Map FilePath GHC.ModSummary -> Diagnostic -> m [FileEdit]
suggestRedundantImportEdits modSummariesByFile Diagnostic {diagnosticSpan, diagnosticMessage} =
  case diagnosticSpan of
    RealDiagnosticSpan span'@Span {spanFile} ->
      case parseRedundantImportDiagnostic diagnosticMessage of
        Nothing ->
          pure []
        Just redundantDiagnostic ->
          case Map.lookup (normalise spanFile) modSummariesByFile of
            Nothing ->
              pure []
            Just summary ->
              GHC.handleSourceError
                (const (pure []))
                ( do
                    parsedModule <- GHC.parseModule summary
                    contents <- liftIO $ TIO.readFile spanFile
                    pure (buildEdits parsedModule contents span' redundantDiagnostic)
                )
    UnhelpfulDiagnosticSpan {} ->
      pure []

data ParsedRedundantImportDiagnostic
  = RemoveBindings Text
  | RemoveWholeImport
  deriving (Eq, Show)

buildEdits :: GHC.ParsedModule -> Text -> Span -> ParsedRedundantImportDiagnostic -> [FileEdit]
buildEdits parsedModule contents diagnosticSpan =
  \case
    RemoveBindings bindingsText ->
      case findImportDeclBySpan hsmodImports diagnosticSpan of
        Just importDecl ->
          [ ReplaceSpanEdit
              diagnosticSpan.spanFile
              deletionSpan
              ""
          | deletionSpan <- mkDeletionSpans contents False (concatMap (rangesForBindingImport importDecl . T.unpack) (T.splitOn ", " bindingsText))
          ]
        Nothing ->
          []
    RemoveWholeImport ->
      case findImportDeclSpanBySpan hsmodImports diagnosticSpan of
        Just importSpan ->
          [ ReplaceSpanEdit
              diagnosticSpan.spanFile
              (extendToWholeLineIfPossible contents importSpan)
              ""
          ]
        Nothing ->
          []
  where
    GHC.L _ HsModule {hsmodImports} = GHC.pm_parsed_source parsedModule

parseRedundantImportDiagnostic :: Text -> Maybe ParsedRedundantImportDiagnostic
parseRedundantImportDiagnostic rawMessage
  | " is redundant" `T.isInfixOf` message =
      parseSpecificRedundantImport message <|> parseWholeImport message
  | otherwise =
      Nothing
  where
    message = unifySpaces rawMessage

parseSpecificRedundantImport :: Text -> Maybe ParsedRedundantImportDiagnostic
parseSpecificRedundantImport message = do
  suffix <- T.stripPrefix "The import of " message <|> T.stripPrefix "The qualified import of " message
  quoted <- extractQuoted suffix
  afterQuoted <- snd <$> nonEmptyBreak " from module " suffix
  if " is redundant" `T.isInfixOf` afterQuoted
    then Just (RemoveBindings quoted)
    else Nothing

parseWholeImport :: Text -> Maybe ParsedRedundantImportDiagnostic
parseWholeImport message
  | "The import of " `T.isInfixOf` message = Just RemoveWholeImport
  | "The qualified import of " `T.isInfixOf` message = Just RemoveWholeImport
  | otherwise = Nothing

findImportDeclBySpan :: [LImportDecl GhcPs] -> Span -> Maybe (ImportDecl GhcPs)
findImportDeclBySpan imports importSpan =
  GHC.unLoc <$> findImportDecl imports importSpan

findImportDeclSpanBySpan :: [LImportDecl GhcPs] -> Span -> Maybe Span
findImportDeclSpanBySpan imports importSpan =
  findImportDecl imports importSpan >>= srcSpanToSpan . GHC.locA . GHC.getLoc

findImportDecl :: [LImportDecl GhcPs] -> Span -> Maybe (LImportDecl GhcPs)
findImportDecl imports importSpan =
  find importContainsSpan imports
  where
    importContainsSpan locatedImport =
      maybe False (`spanContains` importSpan) (srcSpanToSpan (GHC.locA (GHC.getLoc locatedImport)))

mkDeletionSpans :: Text -> Bool -> [Span] -> [Span]
mkDeletionSpans contents acceptNoComma ranges =
  normalizeDeletionSpans contents $
    mapMaybe (extendToIncludeCommaIfPossible acceptNoComma contents) ranges

normalizeDeletionSpans :: Text -> [Span] -> [Span]
normalizeDeletionSpans contents =
  mergeSpans contents
    . nubBy (==)
    . sortBy (\left right -> compare (spanStartKey left) (spanStartKey right))

mergeSpans :: Text -> [Span] -> [Span]
mergeSpans _ [] = []
mergeSpans contents (current : rest) =
  go current rest
  where
    go acc [] = [acc]
    go acc (next : remaining)
      | spansOverlapOrTouch contents acc next =
          go (mergeTwoSpans acc next) remaining
      | otherwise =
          acc : go next remaining

spansOverlapOrTouch :: Text -> Span -> Span -> Bool
spansOverlapOrTouch contents left right =
  case (spanToOffsets contents left, spanToOffsets contents right) of
    (Just (leftStart, leftEnd), Just (rightStart, rightEnd)) ->
      leftEnd >= rightStart || rightEnd >= leftStart
    _ ->
      False

mergeTwoSpans :: Span -> Span -> Span
mergeTwoSpans left right =
  Span
    { spanFile = left.spanFile,
      spanStartLine = min left.spanStartLine right.spanStartLine,
      spanStartCol =
        if spanStartKey left <= spanStartKey right
          then left.spanStartCol
          else right.spanStartCol,
      spanEndLine = max left.spanEndLine right.spanEndLine,
      spanEndCol =
        if spanEndKey left >= spanEndKey right
          then left.spanEndCol
          else right.spanEndCol
    }

extendToWholeLineIfPossible :: Text -> Span -> Span
extendToWholeLineIfPossible contents span'
  | span'.spanStartCol /= 1 = span'
  | Just (_, endOffset) <- spanToOffsets contents span',
    let after = dropText endOffset contents,
    let (spaces, rest) = T.span (\ch -> isSpace ch && ch /= '\n') after,
    not (T.null rest),
    T.head rest == '\n',
    Just (newEndLine, newEndCol) <- offsetToPosition contents (endOffset + T.length spaces + 1) =
      span' {spanEndLine = newEndLine, spanEndCol = newEndCol}
  | otherwise = span'

extendToIncludeCommaIfPossible :: Bool -> Text -> Span -> Maybe Span
extendToIncludeCommaIfPossible acceptNoComma contents span' = do
  (startOffset, endOffset) <- spanToOffsets contents span'
  let before = takeText startOffset contents
      after = dropText endOffset contents
      beforeTrimmed = T.dropWhileEnd isSpace before
      afterTrimmed = T.dropWhile isSpace after
  case () of
    _
      | Just ',' <- snd <$> T.unsnoc beforeTrimmed -> do
          let newStartOffset = T.length beforeTrimmed - 1
          (newStartLine, newStartCol) <- offsetToPosition contents newStartOffset
          pure span' {spanStartLine = newStartLine, spanStartCol = newStartCol}
      | Just (',', restAfterComma) <- T.uncons afterTrimmed -> do
          let skippedLeading = T.length after - T.length afterTrimmed
              skippedAfterComma = T.length restAfterComma - T.length (T.dropWhile isSpace restAfterComma)
              newEndOffset = endOffset + skippedLeading + 1 + skippedAfterComma
          (newEndLine, newEndCol) <- offsetToPosition contents newEndOffset
          pure span' {spanEndLine = newEndLine, spanEndCol = newEndCol}
      | acceptNoComma ->
          pure span'
      | otherwise ->
          Nothing

rangesForBindingImport :: ImportDecl GhcPs -> String -> [Span]
rangesForBindingImport importDecl bindingName =
  case ideclImportList importDecl of
    Just (GHC.Exactly, GHC.L _ lies) ->
      concatMap (mapMaybe srcSpanToSpan . rangesForBinding' bindingName) lies
    _ ->
      []

wrapOperatorInParens :: String -> String
wrapOperatorInParens identifier =
  case identifier of
    '(' : _
      | last identifier == ')' -> identifier
    '_' : _ -> identifier
    headChar : _
      | isAlphaNumUnderscore headChar -> identifier
      | otherwise -> "(" <> identifier <> ")"
    [] -> ""
  where
    isAlphaNumUnderscore ch =
      ch == '_' || ch == '\'' || elem ch ['a' .. 'z'] || elem ch ['A' .. 'Z'] || elem ch ['0' .. '9']

rangesForBinding' :: String -> LIE GhcPs -> [GHC.SrcSpan]
rangesForBinding' bindingName (GHC.L located ie) =
  case ie of
    IEVar _ wrappedName
      | matchesWrappedName bindingName wrappedName ->
          [GHC.locA located]
    IEThingAbs _ wrappedName
      | matchesWrappedName bindingName wrappedName ->
          [GHC.locA located]
    IEThingAll _ wrappedName
      | matchesWrappedName bindingName wrappedName || renderOutputable ie == bindingName ->
          [GHC.locA located]
    IEThingWith _ wrappedName _ subNames
      | matchesWrappedName bindingName wrappedName || renderOutputable ie == bindingName ->
          [GHC.locA located]
      | otherwise ->
          [ GHC.locA (GHC.getLoc subName)
          | subName <- subNames,
            matchesWrappedName bindingName subName
          ]
    _ ->
      []

matchesWrappedName :: String -> GHC.LIEWrappedName GhcPs -> Bool
matchesWrappedName bindingName wrappedName =
  case GHC.unLoc wrappedName of
    IEPattern _ patternName ->
      matchesRenderedName (renderOutputable (GHC.unLoc patternName)) bindingName
        || ("pattern " <> renderOutputable (GHC.unLoc patternName)) == bindingName
    IEName _ name ->
      matchesRenderedName (renderOutputable (GHC.unLoc name)) bindingName
    IEType _ name ->
      matchesRenderedName (renderOutputable (GHC.unLoc name)) bindingName

matchesRenderedName :: String -> String -> Bool
matchesRenderedName actual bindingName =
  actual == bindingName
    || wrapOperatorInParens actual == bindingName
    || actual == wrapOperatorInParens bindingName

renderOutputable :: (Outputable.Outputable a) => a -> String
renderOutputable =
  Outputable.showSDocUnsafe . Outputable.ppr

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

spanContains :: Span -> Span -> Bool
spanContains outer inner =
  outer.spanFile == inner.spanFile
    && spanStartKey outer <= spanStartKey inner
    && spanEndKey outer >= spanEndKey inner

spanStartKey :: Span -> (Int, Int)
spanStartKey Span {spanStartLine, spanStartCol} = (spanStartLine, spanStartCol)

spanEndKey :: Span -> (Int, Int)
spanEndKey Span {spanEndLine, spanEndCol} = (spanEndLine, spanEndCol)

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

takeText :: Int -> Text -> Text
takeText =
  T.take

dropText :: Int -> Text -> Text
dropText =
  T.drop

extractQuoted :: Text -> Maybe Text
extractQuoted text =
  listToMaybe (quotedSegments text)

quotedSegments :: Text -> [Text]
quotedSegments =
  go []
  where
    go acc remaining =
      case firstQuote remaining of
        Nothing -> reverse acc
        Just (quoteStart, afterOpen) ->
          case T.breakOn (matchingQuote quoteStart) afterOpen of
            (segment, afterClose)
              | T.null afterClose -> reverse acc
              | otherwise ->
                  go (segment : acc) (T.drop 1 afterClose)

firstQuote :: Text -> Maybe (Char, Text)
firstQuote text =
  case T.findIndex isQuoteChar text of
    Nothing -> Nothing
    Just index ->
      let quoteStart = T.index text index
       in Just (quoteStart, T.drop (index + 1) text)

matchingQuote :: Char -> Text
matchingQuote quoteStart =
  T.singleton $
    case quoteStart of
      '‘' -> '’'
      '`' -> '\''
      '\'' -> '\''
      '"' -> '"'
      other -> other

isQuoteChar :: Char -> Bool
isQuoteChar ch =
  ch == '‘' || ch == '`' || ch == '\'' || ch == '"'

nonEmptyBreak :: Text -> Text -> Maybe (Text, Text)
nonEmptyBreak needle haystack =
  let pair@(_, suffix) = T.breakOn needle haystack
   in if T.null suffix then Nothing else Just pair

unifySpaces :: Text -> Text
unifySpaces =
  T.unwords . T.words
