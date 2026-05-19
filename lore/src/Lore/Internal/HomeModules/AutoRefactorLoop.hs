module Lore.Internal.HomeModules.AutoRefactorLoop
  ( maxAutoRefactorApplications,
    loadHomeModulesWithAutoRefactorRetries,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.HomeModules.LoadAttempt (HomeModulesLoadAttempt (..), loadHomeModulesOnce)
import Lore.Internal.HomeModules.Plan (HomeModulesLoadPlan)
import Lore.Internal.ImportCleanup.Apply (ImportCleanupApplyResult (..), applyImportCleanupFromDiagnostics)
import Lore.Internal.Session.CacheInvalidation (invalidateCachesAfterSourceEdits)
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
        plan
  | otherwise =
      loadHomeModulesOnce plan

runAutoRefactorRetryLoop ::
  (MonadLore m) =>
  Int ->
  HomeModulesLoadPlan ->
  m HomeModulesLoadAttempt
runAutoRefactorRetryLoop maxApplications plan =
  go 0 Set.empty Map.empty Set.empty
  where
    go applicationsCount cleanedFiles cleanedSummaryByFile seenSignatures = do
      attempt@HomeModulesLoadAttempt {homeModulesLoadAttemptResult} <- loadHomeModulesOnce plan
      case homeModulesLoadAttemptResult of
        GHC.Succeeded ->
          pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
        GHC.Failed
          | applicationsCount >= maxApplications -> do
              Log.info "Auto-refactor: reached max redundant import cleanup attempts."
              pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
          | otherwise -> do
              cleanupResult <-
                applyImportCleanupFromDiagnostics
                  attempt.homeModulesLoadAttemptModuleSummariesByFile
                  attempt.homeModulesLoadAttemptDiagnostics
              let mergedCleanedFiles =
                    cleanedFiles `Set.union` Set.fromList cleanupResult.importCleanupChangedFiles
                  mergedSummaryByFile =
                    Map.unionWith (<>) cleanedSummaryByFile cleanupResult.importCleanupSummaryByFile
                  cleanupSignature = show (Map.toAscList cleanupResult.importCleanupSignature)
              if cleanupResult.importCleanupApplied
                then do
                  if cleanupSignature `Set.member` seenSignatures
                    then do
                      Log.info "Auto-refactor: stopping retry loop because cleanup signature repeated."
                      pure (withAutoRefactInfo mergedCleanedFiles mergedSummaryByFile attempt)
                    else do
                      Log.info "Auto-refactor: redundant import cleanup was applied. Retrying home-module load."
                      invalidateCachesAfterSourceEdits
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
