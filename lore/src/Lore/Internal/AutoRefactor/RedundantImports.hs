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
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span)
import Lore.Internal.AutoRefactor.ImportDecl (ParsedImport (..), parsedImportContainsSpan)
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..))

data RedundantImportRequest
  = RemoveBindingsRequest Span (NonEmpty T.Text)
  | RemoveWholeImportRequest Span
  deriving (Eq, Show)

redundantImportRequestFromDiagnostic :: Diagnostic -> Maybe RedundantImportRequest
redundantImportRequestFromDiagnostic Diagnostic {diagnosticSpan = RealDiagnosticSpan diagnosticSpan, diagnosticMessage} =
  case parseRedundantImportDiagnostic diagnosticMessage of
    Just RemoveWholeImportDiagnostic ->
      Just (RemoveWholeImportRequest diagnosticSpan)
    Just (RemoveBindings bindings) ->
      Just (RemoveBindingsRequest diagnosticSpan bindings)
    Nothing ->
      Nothing
redundantImportRequestFromDiagnostic Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
  Nothing

suggestRedundantImportOperations :: [ParsedImport] -> NonEmpty RedundantImportRequest -> [ImportOperation]
suggestRedundantImportOperations parsedImports =
  concatMap suggestForRequest . NE.toList
  where
    suggestForRequest = \case
      RemoveWholeImportRequest diagnosticSpan ->
        case find (`parsedImportContainsSpan` diagnosticSpan) parsedImports of
          Just parsedImport ->
            [RemoveWholeImport parsedImport.parsedImportId]
          Nothing ->
            []
      RemoveBindingsRequest diagnosticSpan bindings ->
        case find (`parsedImportContainsSpan` diagnosticSpan) parsedImports of
          Just parsedImport ->
            [ RemoveImportItem parsedImport.parsedImportId bindingText
            | bindingText <- NE.toList bindings
            ]
          Nothing ->
            []

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
