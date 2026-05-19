module Lore.Internal.AutoRefactor
  ( AutoRefactorResult (..),
    applyAutoRefactor,
    applyAutoRefactorFromDiagnostics,
    applyAutoRefactorWithDiagnostics,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified GHC
import Lore.Diagnostics (Diagnostic)
import Lore.Internal.ImportCleanup.Apply
  ( ImportCleanupApplyResult (..),
    applyImportCleanup,
    applyImportCleanupFromDiagnostics,
    applyImportCleanupWithDiagnostics,
  )
import Lore.Internal.ImportCleanup.Types
  ( AutoRefactorIssue (..),
  )
import Lore.Internal.SourceEdit (FileEdit)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data AutoRefactorResult = AutoRefactorResult
  { autoRefactorApplied :: Bool,
    autoRefactorChangedFiles :: [FilePath],
    autoRefactorSummaryByFile :: Map.Map FilePath [String],
    autoRefactorCleanupSignature :: Map.Map FilePath [FileEdit]
  }

applyAutoRefactor ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  NonEmpty AutoRefactorIssue ->
  m AutoRefactorResult
applyAutoRefactor modSummariesByFile issues = do
  Log.debug "Auto-refactor: diagnostics were not provided; using preclassified import-cleanup issues."
  ImportCleanupApplyResult
    { importCleanupApplied,
      importCleanupChangedFiles,
      importCleanupSummaryByFile,
      importCleanupSignature
    } <-
    applyImportCleanup modSummariesByFile issues

  pure
    AutoRefactorResult
      { autoRefactorApplied = importCleanupApplied,
        autoRefactorChangedFiles = importCleanupChangedFiles,
        autoRefactorSummaryByFile = importCleanupSummaryByFile,
        autoRefactorCleanupSignature = importCleanupSignature
      }

applyAutoRefactorWithDiagnostics ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  [Diagnostic] ->
  NonEmpty AutoRefactorIssue ->
  m AutoRefactorResult
-- Diagnostics are authoritative when they classify successfully;
-- supplied issues are used as fallback when diagnostics classify to Nothing.
applyAutoRefactorWithDiagnostics modSummariesByFile diagnostics issues = do
  ImportCleanupApplyResult
    { importCleanupApplied,
      importCleanupChangedFiles,
      importCleanupSummaryByFile,
      importCleanupSignature
    } <-
    applyImportCleanupWithDiagnostics modSummariesByFile diagnostics issues

  pure
    AutoRefactorResult
      { autoRefactorApplied = importCleanupApplied,
        autoRefactorChangedFiles = importCleanupChangedFiles,
        autoRefactorSummaryByFile = importCleanupSummaryByFile,
        autoRefactorCleanupSignature = importCleanupSignature
      }

applyAutoRefactorFromDiagnostics ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  [Diagnostic] ->
  m AutoRefactorResult
applyAutoRefactorFromDiagnostics modSummariesByFile diagnostics = do
  ImportCleanupApplyResult
    { importCleanupApplied,
      importCleanupChangedFiles,
      importCleanupSummaryByFile,
      importCleanupSignature
    } <-
    applyImportCleanupFromDiagnostics modSummariesByFile diagnostics

  pure
    AutoRefactorResult
      { autoRefactorApplied = importCleanupApplied,
        autoRefactorChangedFiles = importCleanupChangedFiles,
        autoRefactorSummaryByFile = importCleanupSummaryByFile,
        autoRefactorCleanupSignature = importCleanupSignature
      }
