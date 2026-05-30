module Lore.Internal.HomeModules
  ( LoadHomeModulesResult (..),
    LoadHomeModulesOptions (..),
    defaultLoadHomeModulesOptions,
    lookupLastLoadHomeModulesResultCache,
    storeLastLoadHomeModulesResultCache,
    loadHomeModules,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Ghc.DynFlags (modifySessionDynFlagsM, setGhcOptionsAndExtensions, setGhcSourceDirs, setPackageEnvironmentM)
import Lore.Internal.HomeModules.AutoRefactorLoop (loadHomeModulesWithAutoRefactorRetries)
import Lore.Internal.HomeModules.LoadAttempt (HomeModulesLoadAttempt (..), countLoadedHomeModules)
import Lore.Internal.HomeModules.Plan
  ( HomeModulesLoadConfig (..),
    HomeModulesLoadPlan (..),
    HomeModulesSelection (..),
    homeModulesSelectionTotal,
    prepareHomeModulesLoadInputs,
    prepareHomeModulesLoadPlan,
  )
import Lore.Internal.HomeModules.Result (LoadHomeModulesResult (..))
import Lore.Internal.Interpreter (refreshInterpreterContext)
import Lore.Internal.Lookup.SymbolsMap (setSymbolsDependencySetCache)
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (LastLoadHomeModulesResultCache (..))
import Lore.Internal.Session.CacheInvalidation (invalidateCachesForHomeModuleConfigurationChange)
import Lore.Internal.SourceText (relativeSourcePath)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data LoadHomeModulesOptions = LoadHomeModulesOptions
  { enableAutoRefactor :: Bool
  }

defaultLoadHomeModulesOptions :: LoadHomeModulesOptions
defaultLoadHomeModulesOptions =
  LoadHomeModulesOptions
    { enableAutoRefactor = False
    }

lookupLastLoadHomeModulesResultCache :: (MonadLore m) => m (Maybe LoadHomeModulesResult)
lookupLastLoadHomeModulesResultCache = do
  cachedResultVar <- asks lastLoadHomeModulesResultCacheVar
  LastLoadHomeModulesResultCache maybeLoadHomeModulesResult <- liftIO (MVar.readMVar cachedResultVar)
  pure maybeLoadHomeModulesResult

storeLastLoadHomeModulesResultCache :: (MonadLore m) => LoadHomeModulesResult -> m ()
storeLastLoadHomeModulesResultCache loadHomeModulesResult = do
  cachedResultVar <- asks lastLoadHomeModulesResultCacheVar
  liftIO $
    MVar.modifyMVar_ cachedResultVar $
      const (pure (LastLoadHomeModulesResultCache (Just loadHomeModulesResult)))

loadHomeModules :: (MonadLore m) => LoadHomeModulesOptions -> m LoadHomeModulesResult
loadHomeModules options = do
  inputs <- prepareHomeModulesLoadInputs
  plan <- prepareHomeModulesLoadPlan inputs
  configureHomeModulesSession plan
  attempt <- runHomeModulesLoad options plan
  result <- finalizeHomeModulesLoad plan attempt
  storeLastLoadHomeModulesResultCache result
  pure result

configureHomeModulesSession :: (MonadLore m) => HomeModulesLoadPlan -> m ()
configureHomeModulesSession plan = do
  logHomeModulesLoadPlanDetails plan
  invalidateCachesForHomeModuleConfigurationChange
  setSymbolsDependencySetCache plan.homeModulesLoadConfig.homeModulesPackageEnvironmentCacheKey
  modifySessionDynFlagsM
    ( setGhcOptionsAndExtensions
        plan.homeModulesLoadConfig.homeModulesCommonLanguage
        (Set.toList plan.homeModulesLoadConfig.homeModulesCommonGhcOptions)
        (Set.toList plan.homeModulesLoadConfig.homeModulesCommonExtensions)
        . setGhcSourceDirs (Set.toList plan.homeModulesLoadConfig.homeModulesSourceDirs)
        <=< setPackageEnvironmentM plan.homeModulesLoadConfig.homeModulesPackageEnvironment
    )
  GHC.setTargets plan.homeModulesSelection.ghcTargets

runHomeModulesLoad ::
  (MonadLore m) =>
  LoadHomeModulesOptions ->
  HomeModulesLoadPlan ->
  m HomeModulesLoadAttempt
runHomeModulesLoad options plan =
  loadHomeModulesWithAutoRefactorRetries options.enableAutoRefactor plan

finalizeHomeModulesLoad ::
  (MonadLore m) =>
  HomeModulesLoadPlan ->
  HomeModulesLoadAttempt ->
  m LoadHomeModulesResult
finalizeHomeModulesLoad plan attempt = do
  -- Refresh even after failed loads: a partial module graph can still be useful.
  refreshInterpreterContext

  loadedModulesCount <-
    countLoadedHomeModules
      plan.homeModulesSelection.namedHomeModules
      plan.homeModulesSelection.fileHomeModuleSources

  let totalModulesCount = homeModulesSelectionTotal plan.homeModulesSelection
      failedModulesCount = totalModulesCount - loadedModulesCount

  projectRootPath <- asks projectRoot
  let displayPath = relativeSourcePath projectRootPath
      autofixedFilesDisplay = map displayPath (Set.toAscList attempt.homeModulesLoadAttemptAutoRefactFiles)
      autofixSummaryDisplay =
        [ (displayPath filePath, summaryLines)
        | (filePath, summaryLines) <- attempt.homeModulesLoadAttemptAutoRefactSummaryByFile
        ]

  case attempt.homeModulesLoadAttemptResult of
    GHC.Succeeded ->
      Log.debug "Successfully updated GHC targets based on discovered package configurations"
    GHC.Failed ->
      Log.err "Failed to load GHC targets after updating. Please check the provided GHC options, source directories, and dependencies for correctness."

  pure
    LoadHomeModulesResult
      { loadHomeModulesDiagnostics = attempt.homeModulesLoadAttemptDiagnostics,
        loadHomeModulesSucceeded =
          case attempt.homeModulesLoadAttemptResult of
            GHC.Succeeded -> True
            GHC.Failed -> False,
        loadHomeModulesLoaded = loadedModulesCount,
        loadHomeModulesFailed = failedModulesCount,
        loadHomeModulesAutofixed = Set.size attempt.homeModulesLoadAttemptAutoRefactFiles,
        loadHomeModulesAutofixedFiles = autofixedFilesDisplay,
        loadHomeModulesAutofixSummaryByFile = autofixSummaryDisplay,
        loadHomeModulesTotal = totalModulesCount
      }

logHomeModulesLoadPlanDetails :: (MonadLore m) => HomeModulesLoadPlan -> m ()
logHomeModulesLoadPlanDetails plan = do
  Log.debug $ "Source directories to add: " <> show (Set.toList plan.homeModulesLoadConfig.homeModulesSourceDirs)
  Log.debug $ "Common language: " <> show plan.homeModulesLoadConfig.homeModulesCommonLanguage
  Log.debug $ "Common GHC options: " <> show (Set.toList plan.homeModulesLoadConfig.homeModulesCommonGhcOptions)
  Log.debug $ "Common extensions: " <> show (Set.toList plan.homeModulesLoadConfig.homeModulesCommonExtensions)
  Log.debug $ "Dependency names: " <> show (Set.toList plan.homeModulesLoadConfig.homeModulesDependencyNames)
  Log.debug $ "Package environment cache key: " <> show (Set.toList plan.homeModulesLoadConfig.homeModulesPackageEnvironmentCacheKey)
