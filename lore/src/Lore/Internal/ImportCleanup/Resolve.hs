{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.ImportCleanup.Resolve
  ( resolveImportCleanupGroups,
    findImportByDiagnosticSpan,
  )
where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lore.Internal.ImportCleanup.Types
  ( ImportCleanupAction (..),
    ImportCleanupWarning (..),
    ImportId,
    ParsedImport (..),
    ParsedImportListKind (..),
    RedundantImportIssue (..),
    RedundantImportedOccurrence,
    redundantImportIssueSpan,
  )
import Lore.Internal.SourceSpan (spanContains)
import Lore.Internal.SourceSpan.Types (Span)

resolveImportCleanupGroups ::
  [ParsedImport] ->
  NonEmpty RedundantImportIssue ->
  (Map.Map ImportId ImportCleanupAction, [ImportCleanupWarning])
resolveImportCleanupGroups parsedImports issues =
  foldl step (Map.empty, []) (NE.toList issues)
  where
    step (groups, warnings) issue =
      case assignIssue parsedImports issue of
        Left warning ->
          (groups, warnings <> [warning])
        Right (importKey, groupUpdate) ->
          (Map.insertWith mergeAction importKey groupUpdate groups, warnings)

assignIssue ::
  [ParsedImport] ->
  RedundantImportIssue ->
  Either ImportCleanupWarning (ImportId, ImportCleanupAction)
assignIssue parsedImports issue = do
  parsedImport <- resolveUniqueImport parsedImports (redundantImportIssueSpan issue)
  case issue of
    RedundantWholeImportIssue _ ->
      Right
        ( parsedImport.parsedImportId,
          DeleteImport parsedImport
        )
    RedundantImportOccurrencesIssue _ issueOccurrences ->
      case parsedImport.parsedImportListKind of
        ParsedOpenImport ->
          Left (ImportListRequiredForItemCleanup parsedImport.parsedImportId)
        ParsedHidingImport ->
          Left (HidingImportItemCleanupUnsupported parsedImport.parsedImportId)
        ParsedExplicitImport ->
          Right
            ( parsedImport.parsedImportId,
              RemoveImportOccurrences parsedImport issueOccurrences
            )

mergeAction :: ImportCleanupAction -> ImportCleanupAction -> ImportCleanupAction
mergeAction newer older =
  case (older, newer) of
    (DeleteImport parsedImport, _) ->
      DeleteImport parsedImport
    (_, DeleteImport parsedImport) ->
      DeleteImport parsedImport
    (RemoveImportOccurrences parsedImport leftOccurrences, RemoveImportOccurrences _ rightOccurrences) ->
      RemoveImportOccurrences parsedImport (dedupeOccurrences (leftOccurrences <> rightOccurrences))

dedupeOccurrences :: NonEmpty RedundantImportedOccurrence -> NonEmpty RedundantImportedOccurrence
dedupeOccurrences occurrences =
  case deduped of
    firstOccurrence : remainingOccurrences ->
      firstOccurrence NE.:| remainingOccurrences
    [] ->
      occurrences
  where
    deduped =
      reverse
        (snd (foldl step (Set.empty, []) (NE.toList occurrences)))

    step (seenOccurrences, reversedUniqueOccurrences) occurrence
      | Set.member occurrence seenOccurrences =
          (seenOccurrences, reversedUniqueOccurrences)
      | otherwise =
          (Set.insert occurrence seenOccurrences, occurrence : reversedUniqueOccurrences)

resolveUniqueImport :: [ParsedImport] -> Span -> Either ImportCleanupWarning ParsedImport
resolveUniqueImport parsedImports diagnosticSpan =
  case matchingImports of
    [] -> Left (NoMatchingImportForDiagnostic diagnosticSpan)
    [one] -> Right one
    _ -> Left (AmbiguousDiagnosticImportMatch diagnosticSpan)
  where
    matchingImports =
      [ parsedImport
      | parsedImport <- parsedImports,
        spanContains parsedImport.parsedImportSpan diagnosticSpan
      ]

findImportByDiagnosticSpan :: [ParsedImport] -> Span -> Maybe ParsedImport
findImportByDiagnosticSpan parsedImports diagnosticSpan =
  find (\parsedImport -> spanContains parsedImport.parsedImportSpan diagnosticSpan) parsedImports
