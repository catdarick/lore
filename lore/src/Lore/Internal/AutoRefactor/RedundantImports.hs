{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.RedundantImports
  ( RedundantImportRequest (..),
    redundantImportRequestFromDiagnostic,
    suggestRedundantImportOperations,
  )
where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..))
import Lore.Internal.AutoRefactor.ImportDecl (ImportItem (..), ImportList (..), ParsedImport (..), parsedImportContainsSpan)
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..))

data RedundantImportRequest = RedundantImportRequest
  { redundantImportDiagnosticSpan :: Span,
    redundantImportBindings :: Maybe (NonEmpty T.Text)
  }
  deriving (Eq, Show)

redundantImportRequestFromDiagnostic :: Diagnostic -> Maybe RedundantImportRequest
redundantImportRequestFromDiagnostic Diagnostic {diagnosticSpan = RealDiagnosticSpan diagnosticSpan, diagnosticMessage} =
  case parseRedundantImportDiagnostic diagnosticMessage of
    Just RemoveWholeImportDiagnostic ->
      Just
        RedundantImportRequest
          { redundantImportDiagnosticSpan = diagnosticSpan,
            redundantImportBindings = Nothing
          }
    Just (RemoveBindings bindings) ->
      Just
        RedundantImportRequest
          { redundantImportDiagnosticSpan = diagnosticSpan,
            redundantImportBindings = Just bindings
          }
    Nothing ->
      Nothing
redundantImportRequestFromDiagnostic Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
  Nothing

suggestRedundantImportOperations :: [ParsedImport] -> NonEmpty RedundantImportRequest -> [ImportOperation]
suggestRedundantImportOperations parsedImports =
  concatMap suggestForRequest . NE.toList
  where
    suggestForRequest RedundantImportRequest {redundantImportDiagnosticSpan, redundantImportBindings} =
      case find (`parsedImportContainsSpan` redundantImportDiagnosticSpan) parsedImports of
        Nothing ->
          []
        Just parsedImport ->
          case findImportItemBySpan redundantImportDiagnosticSpan parsedImport of
            Just importItem ->
              [RemoveImportItem parsedImport.parsedImportId importItem.importItemText]
            Nothing ->
              fallbackOperations parsedImport redundantImportDiagnosticSpan redundantImportBindings

    fallbackOperations parsedImport diagnosticSpan = \case
      Nothing ->
        [RemoveWholeImport parsedImport.parsedImportId]
      Just bindings
        | spansMatch parsedImport.parsedImportSpan diagnosticSpan ->
            [RemoveWholeImport parsedImport.parsedImportId]
        | otherwise ->
            [ RemoveImportItem parsedImport.parsedImportId bindingText
            | bindingText <- NE.toList bindings
            ]

findImportItemBySpan :: Span -> ParsedImport -> Maybe ImportItem
findImportItemBySpan diagnosticSpan parsedImport =
  find (itemSpanMatches diagnosticSpan) (importItems parsedImport.parsedImportList)

importItems :: ImportList -> [ImportItem]
importItems = \case
  OpenImport -> []
  ExplicitImport items -> items
  HidingImport items -> items

itemSpanMatches :: Span -> ImportItem -> Bool
itemSpanMatches diagnosticSpan importItem =
  case importItem.importItemSpan of
    Nothing -> False
    Just importItemSpan ->
      spansOverlap importItemSpan diagnosticSpan

spansMatch :: Span -> Span -> Bool
spansMatch left right =
  left.spanFile == right.spanFile
    && left.spanStartLine == right.spanStartLine
    && left.spanStartCol == right.spanStartCol
    && left.spanEndLine == right.spanEndLine
    && left.spanEndCol == right.spanEndCol

spansOverlap :: Span -> Span -> Bool
spansOverlap left right =
  left.spanFile == right.spanFile
    && spanStartKey left <= spanEndKey right
    && spanStartKey right <= spanEndKey left

spanStartKey :: Span -> (Int, Int)
spanStartKey Span {spanStartLine, spanStartCol} =
  (spanStartLine, spanStartCol)

spanEndKey :: Span -> (Int, Int)
spanEndKey Span {spanEndLine, spanEndCol} =
  (spanEndLine, spanEndCol)

data ParsedRedundantImportDiagnostic
  = RemoveBindings (NonEmpty T.Text)
  | RemoveWholeImportDiagnostic
  deriving (Eq, Show)

parseRedundantImportDiagnostic :: T.Text -> Maybe ParsedRedundantImportDiagnostic
parseRedundantImportDiagnostic rawMessage
  | " is redundant" `T.isInfixOf` message =
      parseSpecificRedundantImport message <|> parseWholeImport message
  | otherwise =
      Nothing
  where
    message = unifySpaces rawMessage

parseSpecificRedundantImport :: T.Text -> Maybe ParsedRedundantImportDiagnostic
parseSpecificRedundantImport message = do
  suffix <- T.stripPrefix "The import of " message <|> T.stripPrefix "The qualified import of " message
  quoted <- extractQuoted suffix
  afterQuoted <- snd <$> nonEmptyBreak " from module " suffix
  if " is redundant" `T.isInfixOf` afterQuoted
    then RemoveBindings <$> splitBindings quoted
    else Nothing

parseWholeImport :: T.Text -> Maybe ParsedRedundantImportDiagnostic
parseWholeImport message
  | "The import of " `T.isInfixOf` message = Just RemoveWholeImportDiagnostic
  | "The qualified import of " `T.isInfixOf` message = Just RemoveWholeImportDiagnostic
  | otherwise = Nothing

extractQuoted :: T.Text -> Maybe T.Text
extractQuoted text =
  case T.breakOn "‘" text of
    (_, "") -> Nothing
    (_, withOpenQuote) ->
      let afterOpenQuote = T.drop 1 withOpenQuote
          (quoted, afterCloseQuote) = T.breakOn "’" afterOpenQuote
       in if T.null afterCloseQuote
            then Nothing
            else Just quoted

nonEmptyBreak :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
nonEmptyBreak needle haystack =
  let pair@(_, suffix) = T.breakOn needle haystack
   in if T.null suffix then Nothing else Just pair

unifySpaces :: T.Text -> T.Text
unifySpaces =
  T.unwords . T.words

splitBindings :: T.Text -> Maybe (NonEmpty T.Text)
splitBindings =
  NE.nonEmpty . T.splitOn ", "
