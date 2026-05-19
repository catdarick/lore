module Lore.Internal.HomeModules.AutoRefactorLoop
  ( maxAutoRefactorApplications,
    loadHomeModulesWithAutoRefactorRetries,
    runAutoRefactorRetryLoop,
    mergeAutoRefactSummaries,
    applyAutoRefactorFromHomeModulesLoadAttempt,
    emptyAutoRefactorResult,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.AutoRefactor (AutoRefactorResult (..), applyAutoRefactorFromDiagnostics)
import Lore.Internal.HomeModules.LoadAttempt (HomeModulesLoadAttempt (..), loadHomeModulesOnce)
import Lore.Internal.HomeModules.Plan (HomeModulesLoadPlan)
import Lore.Internal.Session.CacheInvalidation (invalidateCachesAfterSourceEdits)
import Lore.Logger (MonadLogger)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

maxAutoRefactorApplications :: Int
maxAutoRefactorApplications = 3

loadHomeModulesWithAutoRefactorRetries ::
  (MonadLore m) =>
  Bool ->
  HomeModulesLoadPlan ->
  m HomeModulesLoadAttempt
loadHomeModulesWithAutoRefactorRetries enableAutoRefactor plan
  | enableAutoRefactor =
      runAutoRefactorRetryLoop
        maxAutoRefactorApplications
        (loadHomeModulesOnce plan)
        applyAutoRefactorFromHomeModulesLoadAttempt
        invalidateCachesAfterSourceEdits
  | otherwise =
      loadHomeModulesOnce plan

runAutoRefactorRetryLoop ::
  (MonadLogger m) =>
  Int ->
  m HomeModulesLoadAttempt ->
  (HomeModulesLoadAttempt -> m AutoRefactorResult) ->
  m () ->
  m HomeModulesLoadAttempt
runAutoRefactorRetryLoop maxApplications loadAttemptOnce applyAutoRefactorFromAttempt invalidateAfterSourceEdits =
  go 0 Set.empty Map.empty Set.empty
  where
    go applicationsCount cleanedFiles cleanedSummaryByFile seenSignatures = do
      attempt@HomeModulesLoadAttempt {homeModulesLoadAttemptResult} <- loadAttemptOnce
      case homeModulesLoadAttemptResult of
        GHC.Succeeded ->
          pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
        GHC.Failed
          | applicationsCount >= maxApplications -> do
              Log.info "Auto-refactor: reached max redundant import cleanup attempts."
              pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
          | otherwise -> do
              cleanupResult <- applyAutoRefactorFromAttempt attempt
              let mergedCleanedFiles =
                    cleanedFiles `Set.union` Set.fromList cleanupResult.autoRefactorChangedFiles
                  mergedSummaryByFile =
                    mergeAutoRefactSummaries cleanedSummaryByFile cleanupResult.autoRefactorSummaryByFile
                  cleanupSignature = show (Map.toAscList cleanupResult.autoRefactorCleanupSignature)
              if cleanupResult.autoRefactorApplied
                then do
                  if cleanupSignature `Set.member` seenSignatures
                    then do
                      Log.info "Auto-refactor: stopping retry loop because cleanup signature repeated."
                      pure (withAutoRefactInfo mergedCleanedFiles mergedSummaryByFile attempt)
                    else do
                      Log.info "Auto-refactor: redundant import cleanup was applied. Retrying home-module load."
                      invalidateAfterSourceEdits
                      go
                        (applicationsCount + 1)
                        mergedCleanedFiles
                        mergedSummaryByFile
                        (Set.insert cleanupSignature seenSignatures)
                else do
                  Log.info "Auto-refactor: skipped retry because cleanup produced no safe edits."
                  pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)

    withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt =
      attempt
        { homeModulesLoadAttemptAutoRefactFiles = cleanedFiles,
          homeModulesLoadAttemptAutoRefactSummaryByFile = Map.toAscList cleanedSummaryByFile
        }

mergeAutoRefactSummaries ::
  Map.Map FilePath [String] ->
  Map.Map FilePath [String] ->
  Map.Map FilePath [String]
mergeAutoRefactSummaries =
  Map.unionWith (<>)

applyAutoRefactorFromHomeModulesLoadAttempt ::
  (MonadLore m) =>
  HomeModulesLoadAttempt ->
  m AutoRefactorResult
applyAutoRefactorFromHomeModulesLoadAttempt HomeModulesLoadAttempt {homeModulesLoadAttemptDiagnostics, homeModulesLoadAttemptModuleSummariesByFile} =
  applyAutoRefactorFromDiagnostics homeModulesLoadAttemptModuleSummariesByFile homeModulesLoadAttemptDiagnostics

emptyAutoRefactorResult :: AutoRefactorResult
emptyAutoRefactorResult =
  AutoRefactorResult
    { autoRefactorApplied = False,
      autoRefactorChangedFiles = [],
      autoRefactorSummaryByFile = Map.empty,
      autoRefactorCleanupSignature = Map.empty
    }
