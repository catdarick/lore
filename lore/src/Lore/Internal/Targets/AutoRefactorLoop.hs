module Lore.Internal.Targets.AutoRefactorLoop
  ( maxAutoRefactorApplications,
    loadTargetsWithAutoRefactorRetries,
    runAutoRefactorRetryLoop,
    mergeAutoRefactSummaries,
    applyAutoRefactorFromDiagnostics,
    emptyAutoRefactorResult,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Diagnostics (Diagnostic)
import Lore.Internal.AutoRefactor (AutoRefactorResult (..), applyAutoRefactor)
import Lore.Internal.AutoRefactor.Issue (classifyAutoRefactorIssues)
import Lore.Internal.Session.CacheInvalidation (invalidateCachesAfterSourceEdits)
import Lore.Internal.Targets.LoadAttempt (LoadAttempt (..), loadTargetsOnce)
import Lore.Internal.Targets.Plan (TargetsPlan)
import Lore.Logger (MonadLogger)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

maxAutoRefactorApplications :: Int
maxAutoRefactorApplications = 3

loadTargetsWithAutoRefactorRetries ::
  (MonadLore m) =>
  Bool ->
  TargetsPlan ->
  m LoadAttempt
loadTargetsWithAutoRefactorRetries enableAutoRefactor targetsPlan
  | enableAutoRefactor =
      runAutoRefactorRetryLoop
        maxAutoRefactorApplications
        (loadTargetsOnce targetsPlan)
        applyAutoRefactorFromDiagnostics
        invalidateCachesAfterSourceEdits
  | otherwise =
      loadTargetsOnce targetsPlan

runAutoRefactorRetryLoop ::
  (MonadLogger m) =>
  Int ->
  m LoadAttempt ->
  ([Diagnostic] -> m AutoRefactorResult) ->
  m () ->
  m LoadAttempt
runAutoRefactorRetryLoop maxApplications loadAttemptOnce applyAutoRefactorFromDiags invalidateAfterSourceEdits =
  go 0 Set.empty Map.empty
  where
    go applicationsCount cleanedFiles cleanedSummaryByFile = do
      attempt@LoadAttempt {loadAttemptDiagnostics, loadAttemptResult} <- loadAttemptOnce
      case loadAttemptResult of
        GHC.Succeeded ->
          pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
        GHC.Failed
          | applicationsCount >= maxApplications -> do
              Log.info "Auto-refactor: reached max redundant import cleanup attempts."
              pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
          | otherwise -> do
              cleanupResult <- applyAutoRefactorFromDiags loadAttemptDiagnostics
              if cleanupResult.autoRefactorApplied
                then do
                  Log.info "Auto-refactor: redundant import cleanup was applied. Retrying target load."
                  invalidateAfterSourceEdits
                  go
                    (applicationsCount + 1)
                    (cleanedFiles `Set.union` Set.fromList cleanupResult.autoRefactorChangedFiles)
                    (mergeAutoRefactSummaries cleanedSummaryByFile cleanupResult.autoRefactorSummaryByFile)
                else
                  pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)

    withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt =
      attempt
        { loadAttemptAutoRefactFiles = cleanedFiles,
          loadAttemptAutoRefactSummaryByFile = Map.toAscList cleanedSummaryByFile
        }

mergeAutoRefactSummaries ::
  Map.Map FilePath [String] ->
  Map.Map FilePath [String] ->
  Map.Map FilePath [String]
mergeAutoRefactSummaries =
  Map.unionWith (<>)

applyAutoRefactorFromDiagnostics ::
  (MonadLore m) =>
  [Diagnostic] ->
  m AutoRefactorResult
applyAutoRefactorFromDiagnostics diagnostics =
  case classifyAutoRefactorIssues diagnostics of
    Nothing -> do
      Log.debug "Auto-refactor: no redundant import diagnostics found; skipping."
      pure emptyAutoRefactorResult
    Just issues ->
      applyAutoRefactor issues

emptyAutoRefactorResult :: AutoRefactorResult
emptyAutoRefactorResult =
  AutoRefactorResult
    { autoRefactorApplied = False,
      autoRefactorChangedFiles = [],
      autoRefactorSummaryByFile = Map.empty
    }
