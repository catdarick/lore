{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Internal.AutoRefact.RedundantImports
  ( suggestRedundantImportOperations,
  )
where

import Control.Applicative ((<|>))
import Data.List (find)
import qualified Data.Text as T
import Internal.AutoRefact.ImportDecl (ParsedImport (..), parsedImportContainsSpan)
import Internal.AutoRefact.ImportOps (ImportOperation (..))
import Internal.Diagnostics (Diagnostic (..), DiagnosticSpan (..))

suggestRedundantImportOperations :: [ParsedImport] -> [Diagnostic] -> [ImportOperation]
suggestRedundantImportOperations parsedImports =
  concatMap suggestForDiagnostic
  where
    suggestForDiagnostic Diagnostic {diagnosticSpan = RealDiagnosticSpan diagnosticSpan, diagnosticMessage} =
      case parseRedundantImportDiagnostic diagnosticMessage of
        Just RemoveWholeImportDiagnostic ->
          case find (`parsedImportContainsSpan` diagnosticSpan) parsedImports of
            Just parsedImport ->
              [RemoveWholeImport parsedImport.parsedImportId]
            Nothing ->
              []
        Just (RemoveBindings bindingsText) ->
          case find (`parsedImportContainsSpan` diagnosticSpan) parsedImports of
            Just parsedImport ->
              [ RemoveImportItem parsedImport.parsedImportId bindingText
              | bindingText <- T.splitOn ", " bindingsText
              ]
            Nothing ->
              []
        Nothing ->
          []
    suggestForDiagnostic Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
      []

data ParsedRedundantImportDiagnostic
  = RemoveBindings T.Text
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
    then Just (RemoveBindings quoted)
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
