{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.ImportCleanup.Diagnostics
  ( redundantImportIssueFromDiagnostic,
    classifyRedundantImportIssues,
  )
where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified GHC.Driver.Flags as DriverFlags
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span)
import Lore.Internal.ImportCleanup.Types
  ( ImportNamespace (..),
    RedundantImportIssue (..),
    RedundantImportedOccurrence (..),
  )

classifyRedundantImportIssues :: [Diagnostic] -> Maybe (NonEmpty RedundantImportIssue)
classifyRedundantImportIssues diagnostics =
  NE.nonEmpty (mapMaybe redundantImportIssueFromDiagnostic diagnostics)

redundantImportIssueFromDiagnostic :: Diagnostic -> Maybe RedundantImportIssue
redundantImportIssueFromDiagnostic Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
  Nothing
redundantImportIssueFromDiagnostic Diagnostic {diagnosticSpan = RealDiagnosticSpan span', diagnosticWarningFlag, diagnosticMessage}
  | diagnosticWarningFlag /= Just DriverFlags.Opt_WarnUnusedImports =
      Nothing
  | otherwise =
      redundantImportIssueFromMessage span' (unifySpaces diagnosticMessage)

redundantImportIssueFromMessage :: Span -> T.Text -> Maybe RedundantImportIssue
redundantImportIssueFromMessage span' message
  | not (" is redundant" `T.isInfixOf` message) =
      Nothing
  | Just occurrences <- parseOccurrenceDiagnostic message =
      Just (RedundantImportOccurrencesIssue span' occurrences)
  | parseWholeImportDiagnostic message =
      Just (RedundantWholeImportIssue span')
  | otherwise =
      Nothing

parseOccurrenceDiagnostic :: T.Text -> Maybe (NonEmpty RedundantImportedOccurrence)
parseOccurrenceDiagnostic message = do
  suffix <- T.stripPrefix "The import of " message <|> T.stripPrefix "The qualified import of " message
  (quotedOccurrencesText, afterQuoted) <- extractQuotedAndSuffix suffix
  afterFromModule <-
    T.stripPrefix " from module " afterQuoted
      <|> T.stripPrefix "from module " (T.stripStart afterQuoted)
  (_moduleName, trailing) <- breakBeforeRedundantSuffix afterFromModule
  if " is redundant" `T.isPrefixOf` trailing
    then do
      let chunks = splitTopLevelCommaSeparated quotedOccurrencesText
          occurrences = mapMaybe parseOccurrenceChunk chunks
      NE.nonEmpty occurrences
    else Nothing

parseWholeImportDiagnostic :: T.Text -> Bool
parseWholeImportDiagnostic message =
  case parseWholeImportDiagnosticMatch message of
    Just () -> True
    Nothing -> False

parseWholeImportDiagnosticMatch :: T.Text -> Maybe ()
parseWholeImportDiagnosticMatch message = do
  suffix <- T.stripPrefix "The import of " message <|> T.stripPrefix "The qualified import of " message
  (_quotedModuleText, afterQuoted) <- extractQuotedAndSuffix suffix
  let trimmedAfterQuoted = T.stripStart afterQuoted
  if "from module " `T.isPrefixOf` trimmedAfterQuoted
    then Nothing
    else
      if "is redundant" `T.isPrefixOf` trimmedAfterQuoted || " is redundant" `T.isInfixOf` trimmedAfterQuoted
        then Just ()
        else Nothing

breakBeforeRedundantSuffix :: T.Text -> Maybe (T.Text, T.Text)
breakBeforeRedundantSuffix text =
  let (beforeSuffix, suffix) = T.breakOn " is redundant" text
   in if T.null suffix
        then Nothing
        else Just (beforeSuffix, suffix)

extractQuotedAndSuffix :: T.Text -> Maybe (T.Text, T.Text)
extractQuotedAndSuffix text =
  extractBetweenWithSuffix "‘" "’" text
    <|> extractBetweenWithSuffix "`" "'" text

extractBetweenWithSuffix :: T.Text -> T.Text -> T.Text -> Maybe (T.Text, T.Text)
extractBetweenWithSuffix openQuote closeQuote text = do
  (_, withOpenQuote) <- nonEmptyBreak openQuote text
  let afterOpenQuote = T.drop (T.length openQuote) withOpenQuote
      (quoted, afterCloseQuote) = T.breakOn closeQuote afterOpenQuote
  if T.null afterCloseQuote
    then Nothing
    else Just (quoted, T.drop (T.length closeQuote) afterCloseQuote)

parseOccurrenceChunk :: T.Text -> Maybe RedundantImportedOccurrence
parseOccurrenceChunk rawChunk = do
  let chunk = T.strip rawChunk
  if T.null chunk
    then Nothing
    else
      let (namespace, nameText) = parseNamespace chunk
          normalizedName = normalizeOccurrenceName nameText
       in if T.null normalizedName
            then Nothing
            else
              Just
                RedundantImportedOccurrence
                  { redundantOccurrenceText = normalizedName,
                    redundantOccurrenceNamespace = namespace
                  }

parseNamespace :: T.Text -> (Maybe ImportNamespace, T.Text)
parseNamespace text
  | Just stripped <- T.stripPrefix "type " text =
      (Just TypeNamespace, T.strip stripped)
  | Just stripped <- T.stripPrefix "pattern " text =
      (Just PatternNamespace, T.strip stripped)
  | otherwise =
      (Nothing, text)

normalizeOccurrenceName :: T.Text -> T.Text
normalizeOccurrenceName =
  T.strip

nonEmptyBreak :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
nonEmptyBreak needle haystack =
  let pair@(_, suffix) = T.breakOn needle haystack
   in if T.null suffix then Nothing else Just pair

splitTopLevelCommaSeparated :: T.Text -> [T.Text]
splitTopLevelCommaSeparated =
  finalize . T.foldl' step ([], "", 0 :: Int)
  where
    step (chunks, currentChunk, depth) char =
      case char of
        '(' ->
          (chunks, T.snoc currentChunk char, depth + 1)
        ')' ->
          (chunks, T.snoc currentChunk char, max 0 (depth - 1))
        ','
          | depth == 0 ->
              (chunks <> [currentChunk], "", depth)
        _ ->
          (chunks, T.snoc currentChunk char, depth)

    finalize (chunks, currentChunk, _) =
      chunks <> [currentChunk]

unifySpaces :: T.Text -> T.Text
unifySpaces =
  T.unwords . T.words
