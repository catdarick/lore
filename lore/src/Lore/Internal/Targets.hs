{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move filter" #-}
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
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.Make as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Parser.Header as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Unit.Module.Graph as GHC
import GHC.Utils.Monad (mapMaybeM)
import Lore.Diagnostics (Diagnostic (..), driverMessagesToDiagnostics, withDiagnosticsCapturing)
import Lore.Internal.AutoRefactor (AutoRefactorResult (..), applyAutoRefactor)
import Lore.Internal.AutoRefactor.Issue (classifyAutoRefactorIssues)
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..), Language (..), modifySessionDynFlagsM, setDependencies, setGhcOptionsAndExtensions, setGhcSourceDirs)
import Lore.Internal.Interpreter (refreshInterpreterContext)
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.SymbolsMap (setSymbolsDependencySetCache)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Package (ComponentData (..), PackageData (..), defaultExtensions, extractDependencies, extractSourceDirs, prepareComponentsData)
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (LastLoadTargetsResultCache (..))
import Lore.Internal.Session.CacheInvalidation (invalidateCachesAfterSourceEdits, invalidateCachesForTargetConfigurationChange, retainCachesForLoadedModules)
import Lore.Internal.Targets.Result (LoadTargetsResult (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data TargetsPlan = TargetsPlan
  { commonLanguage :: Maybe Language,
    commonExtensions :: Set.Set Extension,
    commonGhcOptions :: Set.Set GhcOption,
    modulesWithComponentOptions :: Map.Map GHC.ModuleName ComponentSpecificOptions
  }

data ComponentSpecificOptions = ComponentSpecificOptions
  { language :: Maybe Language,
    extensions :: Set.Set Extension,
    ghcOptions :: Set.Set GhcOption,
    baseDynFlags :: GHC.DynFlags
  }

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

prepareTargetsPlan :: (MonadLore m) => [ComponentData] -> m TargetsPlan
prepareTargetsPlan components = do
  sessionDynFlags <- GHC.getSessionDynFlags
  let commonLanguage = commonComponentLanguage components
      commonExtensions = foldr1 Set.intersection (map defaultExtensions components)
      commonGhcOptions = foldr1 Set.intersection (map (.ghcOptions) components)

  modulesWithComponentOptions <- forM components \component -> do
    componentFlags <- setGhcOptionsAndExtensions component.language (Set.toList component.ghcOptions) (Set.toList component.defaultExtensions) sessionDynFlags
    let componentSpecificExtensions = component.defaultExtensions Set.\\ commonExtensions
        componentSpecificGhcOptions = component.ghcOptions Set.\\ commonGhcOptions
        componentSpecificLanguage = if component.language == commonLanguage then Nothing else component.language
        componentSpecificOptions =
          ComponentSpecificOptions
            { language = componentSpecificLanguage,
              extensions = componentSpecificExtensions,
              ghcOptions = componentSpecificGhcOptions,
              baseDynFlags = componentFlags
            }
    pure $ Map.fromSet (const componentSpecificOptions) component.modules
  pure
    TargetsPlan
      { commonLanguage = commonLanguage,
        commonExtensions = commonExtensions,
        commonGhcOptions = commonGhcOptions,
        modulesWithComponentOptions = Map.unions modulesWithComponentOptions
      }

loadTargets :: (MonadLore m) => LoadTargetsOptions -> m LoadTargetsResult
loadTargets options = do
  dflags <- GHC.getSessionDynFlags
  let homeUnitId = GHC.homeUnitId_ dflags
  packages <- prepareComponentsData
  let allComponents = concatMap (.components) packages
      localPackageNames = Set.fromList (map (.packageName) packages)
      dependencies = extractDependencies allComponents
      dependenciesToAdd = dependencies Set.\\ localPackageNames
      sourceDirs = Set.unions $ map extractSourceDirs packages
  targetsPlan <- prepareTargetsPlan allComponents
  Log.debug $ "Source directories to add: " <> show (Set.toList sourceDirs)
  Log.debug $ "Common language: " <> show targetsPlan.commonLanguage
  Log.debug $ "Common GHC options: " <> show (Set.toList $ commonGhcOptions targetsPlan)
  Log.debug $ "Common extensions: " <> show (Set.toList $ commonExtensions targetsPlan)
  Log.debug $ "Dependencies to add: " <> show (Set.toList dependenciesToAdd)
  invalidateCachesForTargetConfigurationChange
  setSymbolsDependencySetCache dependenciesToAdd
  modifySessionDynFlagsM
    ( setGhcOptionsAndExtensions targetsPlan.commonLanguage (Set.toList $ commonGhcOptions targetsPlan) (Set.toList $ commonExtensions targetsPlan)
        . setGhcSourceDirs (Set.toList sourceDirs)
        . setDependencies (Set.toList dependenciesToAdd)
    )
  let targetModules = Map.keysSet $ modulesWithComponentOptions targetsPlan
      targets = map (mkModuleTarget homeUnitId) (Set.toList targetModules)
      totalModulesCount = Set.size targetModules
  GHC.setTargets targets
  LoadAttempt {loadAttemptDiagnostics, loadAttemptResult, loadAttemptAutoRefactFiles, loadAttemptAutoRefactSummaryByFile} <- loadTargets' options targetsPlan
  refreshInterpreterContext
  loadedModulesCount <- countLoadedModules targetModules
  let failedModulesCount = totalModulesCount - loadedModulesCount
  case loadAttemptResult of
    GHC.Succeeded -> do
      Log.debug "Successfully updated GHC targets based on package.yaml configurations"
    GHC.Failed -> do
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

logDiagnosticsSummary :: (MonadLore m) => [Diagnostic] -> m ()
logDiagnosticsSummary diagnostics = do
  let diagCount = length diagnostics
      previewCount = 20
      previewLines = take previewCount (map show diagnostics)
  if diagCount == 0
    then
      Log.debug "GHC load completed with no diagnostics."
    else do
      Log.debug $
        "GHC load completed with "
          <> show diagCount
          <> " diagnostics."
      Log.debug $
        "Diagnostics preview (first "
          <> show previewCount
          <> "):\n"
          <> intercalate "\n" previewLines

mkModuleTarget :: GHC.UnitId -> GHC.ModuleName -> GHC.Target
mkModuleTarget unitId modName =
  GHC.Target
    { GHC.targetId = GHC.TargetModule modName,
      GHC.targetAllowObjCode = True,
      GHC.targetUnitId = unitId,
      GHC.targetContents = Nothing
    }

data LoadAttempt = LoadAttempt
  { loadAttemptDiagnostics :: [Diagnostic],
    loadAttemptResult :: GHC.SuccessFlag,
    loadAttemptAutoRefactFiles :: Set.Set FilePath,
    loadAttemptAutoRefactSummaryByFile :: [(FilePath, [String])]
  }

loadTargets' :: (MonadLore m) => LoadTargetsOptions -> TargetsPlan -> m LoadAttempt
loadTargets' options targetsPlan =
  go 0 Set.empty Map.empty
  where
    maxImportCleanupAttempts = 3 :: Int

    go attemptNo cleanedFiles cleanedSummaryByFile = do
      attempt@LoadAttempt {loadAttemptDiagnostics, loadAttemptResult} <-
        loadTargetsOnce targetsPlan
      case loadAttemptResult of
        GHC.Succeeded ->
          pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
        GHC.Failed
          | not options.enableAutoRefactor ->
              pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
          | attemptNo >= maxImportCleanupAttempts -> do
              Log.info "Auto-refact: reached max redundant import cleanup attempts."
              pure (withAutoRefactInfo cleanedFiles cleanedSummaryByFile attempt)
          | otherwise -> do
              cleanupResult <- applyAutoRefactorFromDiagnostics loadAttemptDiagnostics
              if cleanupResult.autoRefactorApplied
                then do
                  Log.info "Auto-refact: redundant import cleanup was applied. Retrying target load."
                  invalidateCachesAfterSourceEdits
                  go
                    (attemptNo + 1)
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
      Log.debug "Auto-refact: no redundant import diagnostics found; skipping."
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

loadTargetsOnce :: (MonadLore m) => TargetsPlan -> m LoadAttempt
loadTargetsOnce targetsPlan = do
  Log.debug "Starting dependency analysis and target loading..."
  (errs, modGraph) <- GHC.depanalE [] False
  let dependencyDiagnostics = driverMessagesToDiagnostics errs
  unless (null errs) $ do
    Log.err $ "Errors during dependency analysis: " <> intercalate "\n" (map show dependencyDiagnostics)
  Log.debug "Patching module graph with component-specific GHC options..."
  patchedModGraph <- applyModuleScopedArgs targetsPlan modGraph
  ifaceCache <- asks ifaceCache
  Log.debug "Loading targets with GHC..."
  (diagnostics, r) <- withDiagnosticsCapturing do
    GHC.load' (Just ifaceCache) GHC.LoadAllTargets Nothing patchedModGraph
  loadedModules <- collectLoadedModules patchedModGraph
  retainCachesForLoadedModules loadedModules
  logDiagnosticsSummary diagnostics
  pure
    LoadAttempt
      { loadAttemptDiagnostics = dependencyDiagnostics <> diagnostics,
        loadAttemptResult = r,
        loadAttemptAutoRefactFiles = Set.empty,
        loadAttemptAutoRefactSummaryByFile = []
      }

collectLoadedModules :: (MonadLore m) => GHC.ModuleGraph -> m (Set.Set GHC.Module)
collectLoadedModules moduleGraph = do
  loaded <- mapMaybeM keepLoadedModule [GHC.ms_mod ms | ms <- GHC.mgModSummaries moduleGraph]
  pure (Set.fromList loaded)
  where
    keepLoadedModule mod' = do
      maybeModuleInfo <- GHC.getModuleInfo mod'
      pure $
        case maybeModuleInfo of
          Just _ -> Just mod'
          Nothing -> Nothing

countLoadedModules :: (MonadLore m) => Set.Set GHC.ModuleName -> m Int
countLoadedModules targetModules = do
  ModSummaries modSummaries <- getCachedModSummaries
  let targetMods =
        [ mod'
        | mod' <- Map.keys modSummaries,
          GHC.moduleName mod' `Set.member` targetModules
        ]
  length <$> mapMaybeM GHC.getModuleInfo targetMods

applyModuleScopedArgs ::
  (MonadLore m) =>
  TargetsPlan ->
  GHC.ModuleGraph ->
  m GHC.ModuleGraph
applyModuleScopedArgs TargetsPlan {modulesWithComponentOptions} modGraph = do
  patchedNodes <- mapM patchNode (GHC.mgModSummaries' modGraph)
  pure (GHC.mkModuleGraph patchedNodes)
  where
    patchNode node =
      case node of
        GHC.ModuleNode deps summary ->
          GHC.ModuleNode deps <$> patchSummary summary
        _ -> pure node
    patchSummary summary =
      let summaryFile =
            fromMaybe
              (GHC.ms_hspp_file summary)
              (GHC.ml_hs_file (GHC.ms_location summary))
          moduleName = GHC.moduleName (GHC.ms_mod summary)
       in case Map.lookup moduleName modulesWithComponentOptions of
            Just componentOptions
              | isJust componentOptions.language
                  || length componentOptions.ghcOptions + length componentOptions.extensions > 0 -> do
                  dynFlags <- applySourcePragmas summary componentOptions summaryFile
                  pure summary {GHC.ms_hspp_opts = dynFlags}
            _ -> pure summary

applySourcePragmas ::
  (MonadLore m) =>
  GHC.ModSummary ->
  ComponentSpecificOptions ->
  FilePath ->
  m GHC.DynFlags
applySourcePragmas summary compOptions summaryFile = do
  contents <-
    case GHC.ms_hspp_buf summary of
      Just buffer -> pure buffer
      Nothing -> liftIO (GHC.hGetStringBuffer summaryFile)
  let (_warnings, options) = GHC.getOptions (GHC.initParserOpts compOptions.baseDynFlags) contents summaryFile
  (dynFlags, _, _) <- liftIO (GHC.parseDynamicFilePragma compOptions.baseDynFlags options)
  pure dynFlags

commonComponentLanguage :: [ComponentData] -> Maybe Language
commonComponentLanguage [] = Nothing
commonComponentLanguage (component : restComponents)
  | all ((== component.language) . (.language)) restComponents = component.language
  | otherwise = Nothing
