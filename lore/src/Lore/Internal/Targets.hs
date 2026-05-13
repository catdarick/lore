module Lore.Internal.Targets
  ( LoadTargetsResult (..),
    LoadTargetsOptions (..),
    defaultLoadTargetsOptions,
    lookupLastLoadTargetsResultCache,
    storeLastLoadTargetsResultCache,
    loadTargets,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Ghc.DynFlags (modifySessionDynFlagsM, setDependencies, setGhcOptionsAndExtensions, setGhcSourceDirs)
import Lore.Internal.Interpreter (refreshInterpreterContext)
import Lore.Internal.Lookup.SymbolsMap (setSymbolsDependencySetCache)
import Lore.Internal.Package (PackageData (..), extractDependencies, extractSourceDirs, prepareComponentsData)
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (LastLoadTargetsResultCache (..))
import Lore.Internal.Session.CacheInvalidation (invalidateCachesForTargetConfigurationChange)
import Lore.Internal.Targets.AutoRefactorLoop (loadTargetsWithAutoRefactorRetries)
import Lore.Internal.Targets.LoadAttempt (LoadAttempt (..), countLoadedModules, mkModuleTarget)
import Lore.Internal.Targets.Plan (TargetsPlan (..), prepareTargetsPlan)
import Lore.Internal.Targets.Result (LoadTargetsResult (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data LoadTargetsOptions = LoadTargetsOptions
  { enableAutoRefactor :: Bool
  }

defaultLoadTargetsOptions :: LoadTargetsOptions
defaultLoadTargetsOptions =
  LoadTargetsOptions
    { enableAutoRefactor = False
    }

lookupLastLoadTargetsResultCache :: (MonadLore m) => m (Maybe LoadTargetsResult)
lookupLastLoadTargetsResultCache = do
  cachedResultVar <- asks lastLoadTargetsResultCacheVar
  LastLoadTargetsResultCache maybeLoadTargetsResult <- liftIO (MVar.readMVar cachedResultVar)
  pure maybeLoadTargetsResult

storeLastLoadTargetsResultCache :: (MonadLore m) => LoadTargetsResult -> m ()
storeLastLoadTargetsResultCache loadTargetsResult = do
  cachedResultVar <- asks lastLoadTargetsResultCacheVar
  liftIO $
    MVar.modifyMVar_ cachedResultVar $
      const (pure (LastLoadTargetsResultCache (Just loadTargetsResult)))

loadTargets :: (MonadLore m) => LoadTargetsOptions -> m LoadTargetsResult
loadTargets options = do
  dflags <- GHC.getSessionDynFlags
  let homeUnitId = GHC.homeUnitId_ dflags
  packages <- prepareComponentsData
  let allComponents = concatMap (.components) packages
      localPackageNames = Set.fromList (map (.packageName) packages)
      dependencies = extractDependencies allComponents
      dependenciesToAdd = dependencies Set.\\ localPackageNames
      sourceDirs = Set.unions (map extractSourceDirs packages)
  targetsPlan <- prepareTargetsPlan allComponents
  logTargetPlanDetails sourceDirs dependenciesToAdd targetsPlan
  invalidateCachesForTargetConfigurationChange
  setSymbolsDependencySetCache dependenciesToAdd
  modifySessionDynFlagsM
    ( setGhcOptionsAndExtensions
        targetsPlan.commonLanguage
        (Set.toList targetsPlan.commonGhcOptions)
        (Set.toList targetsPlan.commonExtensions)
        . setGhcSourceDirs (Set.toList sourceDirs)
        . setDependencies (Set.toList dependenciesToAdd)
    )

  let targetModules = Map.keysSet targetsPlan.modulesWithComponentOptions
      targets = map (mkModuleTarget homeUnitId) (Set.toList targetModules)
      totalModulesCount = Set.size targetModules
  GHC.setTargets targets

  LoadAttempt
    { loadAttemptDiagnostics,
      loadAttemptResult,
      loadAttemptAutoRefactFiles,
      loadAttemptAutoRefactSummaryByFile
    } <-
    loadTargetsWithAutoRefactorRetries options.enableAutoRefactor targetsPlan

  refreshInterpreterContext
  loadedModulesCount <- countLoadedModules targetModules
  let failedModulesCount = totalModulesCount - loadedModulesCount
  case loadAttemptResult of
    GHC.Succeeded ->
      Log.debug "Successfully updated GHC targets based on package.yaml configurations"
    GHC.Failed ->
      Log.err "Failed to load GHC targets after updating. Please check the provided GHC options, source directories, and dependencies for correctness."

  let loadTargetsResult =
        LoadTargetsResult
          { loadTargetsDiagnostics = loadAttemptDiagnostics,
            loadTargetsSucceeded =
              case loadAttemptResult of
                GHC.Succeeded -> True
                GHC.Failed -> False,
            loadTargetsModulesLoaded = loadedModulesCount,
            loadTargetsModulesFailed = failedModulesCount,
            loadTargetsModulesAutofixed = Set.size loadAttemptAutoRefactFiles,
            loadTargetsAutofixedFiles = Set.toAscList loadAttemptAutoRefactFiles,
            loadTargetsAutofixSummaryByFile = loadAttemptAutoRefactSummaryByFile,
            loadTargetsModulesTotal = totalModulesCount
          }
  storeLastLoadTargetsResultCache loadTargetsResult
  pure loadTargetsResult

logTargetPlanDetails ::
  (MonadLore m) =>
  Set.Set FilePath ->
  Set.Set String ->
  TargetsPlan ->
  m ()
logTargetPlanDetails sourceDirs dependenciesToAdd targetsPlan = do
  Log.debug $ "Source directories to add: " <> show (Set.toList sourceDirs)
  Log.debug $ "Common language: " <> show targetsPlan.commonLanguage
  Log.debug $ "Common GHC options: " <> show (Set.toList targetsPlan.commonGhcOptions)
  Log.debug $ "Common extensions: " <> show (Set.toList targetsPlan.commonExtensions)
  Log.debug $ "Dependencies to add: " <> show (Set.toList dependenciesToAdd)
