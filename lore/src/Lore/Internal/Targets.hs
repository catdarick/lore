{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move filter" #-}
module Lore.Internal.Targets
  ( LoadTargetsResult (..),
    LoadTargetsOptions (..),
    defaultLoadTargetsOptions,
    getLastLoadTargetsResult,
    loadTargets,
    retainUnresolvedRollback,
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
import Data.Text (Text)
import qualified GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.Make as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Parser.Header as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Unit.Module.Graph as GHC
import GHC.Utils.Monad (mapMaybeM)
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..), driverMessagesToDiagnostics, withDiagnosticsCapturing)
import Lore.Internal.AutoRefactor (AutoRefactorResult (..), applyAutoRefactor, rollbackAutoRefactorEdits)
import Lore.Internal.AutoRefactor.Issue (classifyAutoRefactorIssues)
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..), Language (..), modifySessionDynFlagsM, setDependencies, setGhcOptionsAndExtensions, setGhcSourceDirs)
import Lore.Internal.Interpreter (invalidateInterpreterContext, refreshInterpreterContext)
import Lore.Internal.Lookup.ModSummaries (getModSummaries, invalidateModSummaries)
import Lore.Internal.Lookup.NameToInstances (invalidateNameToInstancesIndex)
import Lore.Internal.Lookup.SymbolsMap (invalidateSymbolsMapCache)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Package (ComponentData (..), PackageData (..), defaultExtensions, extractDependencies, extractSourceDirs, prepareComponentsData)
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Targets.Result (LoadTargetsResult (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.FilePath (normalise)

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

getLastLoadTargetsResult :: (MonadLore m) => m (Maybe LoadTargetsResult)
getLastLoadTargetsResult = do
  cachedResultVar <- asks lastLoadTargetsResult
  liftIO (MVar.readMVar cachedResultVar)

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
  invalidateInterpreterContext
  invalidateSymbolsMapCache
  invalidateModSummaries
  invalidateNameToInstancesIndex
  modifySessionDynFlagsM
    ( setGhcOptionsAndExtensions targetsPlan.commonLanguage (Set.toList $ commonGhcOptions targetsPlan) (Set.toList $ commonExtensions targetsPlan)
        . setGhcSourceDirs (Set.toList sourceDirs)
        . setDependencies (Set.toList dependenciesToAdd)
    )
  let targetModules = Map.keysSet $ modulesWithComponentOptions targetsPlan
      targets = map (mkModuleTarget homeUnitId) (Set.toList targetModules)
  GHC.setTargets targets
  LoadAttempt {loadAttemptDiagnostics, loadAttemptResult, loadAttemptAutoRefactFiles} <- loadTargets' options targetsPlan
  refreshInterpreterContext
  loadedModulesCount <- countLoadedModules targetModules
  let totalModulesCount = Set.size targetModules
      failedModulesCount = totalModulesCount - loadedModulesCount
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
            loadTargetsModulesTotal = totalModulesCount
          }
  cachedResultVar <- asks lastLoadTargetsResult
  liftIO $
    MVar.modifyMVar_ cachedResultVar $
      const (pure (Just loadTargetsResult))
  pure loadTargetsResult

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
    loadAttemptAutoRefactFiles :: Set.Set FilePath
  }

loadTargets' :: (MonadLore m) => LoadTargetsOptions -> TargetsPlan -> m LoadAttempt
loadTargets' options targetsPlan =
  go 0 Map.empty Set.empty
  where
    maxAutoRefactorAttempts = 3 :: Int

    go attemptNo pendingRollback committedAutoRefactFiles = do
      currentAttempt@LoadAttempt {loadAttemptDiagnostics, loadAttemptResult} <- loadTargetsOnce targetsPlan
      let unresolvedRollback =
            retainUnresolvedRollback pendingRollback loadAttemptDiagnostics
          unresolvedRollbackFiles = rollbackStateFiles unresolvedRollback
          pendingRollbackFiles = rollbackStateFiles pendingRollback
          committedAutoRefactFiles' =
            committedAutoRefactFiles
              `Set.union` (pendingRollbackFiles Set.\\ unresolvedRollbackFiles)
      case loadAttemptResult of
        GHC.Succeeded ->
          pure
            currentAttempt
              { loadAttemptAutoRefactFiles =
                  committedAutoRefactFiles
                    `Set.union` pendingRollbackFiles
              }
        GHC.Failed
          | options.enableAutoRefactor && attemptNo < maxAutoRefactorAttempts -> do
              case classifyAutoRefactorIssues loadAttemptDiagnostics of
                Just autoRefactorIssues -> do
                  AutoRefactorResult {autoRefactorApplied, autoRefactorOriginalContents} <- applyAutoRefactor autoRefactorIssues
                  if autoRefactorApplied
                    then do
                      Log.info "Auto-refact applied import fixes. Retrying target load."
                      invalidateSymbolsMapCache
                      invalidateModSummaries
                      invalidateNameToInstancesIndex
                      go (attemptNo + 1) (Map.union unresolvedRollback autoRefactorOriginalContents) committedAutoRefactFiles'
                    else
                      withAutoRefactFiles committedAutoRefactFiles' <$> rollbackUnresolvedAutoRefact targetsPlan unresolvedRollback currentAttempt
                Nothing -> do
                  Log.debug "Auto-refact: no fixable import diagnostics found; skipping."
                  withAutoRefactFiles committedAutoRefactFiles' <$> rollbackUnresolvedAutoRefact targetsPlan unresolvedRollback currentAttempt
        GHC.Failed ->
          withAutoRefactFiles committedAutoRefactFiles' <$> rollbackUnresolvedAutoRefact targetsPlan unresolvedRollback currentAttempt

    withAutoRefactFiles autoRefactFiles loadAttempt =
      loadAttempt {loadAttemptAutoRefactFiles = autoRefactFiles}

retainUnresolvedRollback :: Map.Map FilePath a -> [Diagnostic] -> Map.Map FilePath a
retainUnresolvedRollback rollbackState diagnostics =
  Map.filterWithKey (\filePath _ -> normalise filePath `Set.member` failingFiles) rollbackState
  where
    failingFiles =
      Set.fromList
        [ normalise spanFile
        | Diagnostic {diagnosticSpan = RealDiagnosticSpan Span {spanFile}} <- diagnostics
        ]

rollbackUnresolvedAutoRefact :: (MonadLore m) => TargetsPlan -> Map.Map FilePath Text -> LoadAttempt -> m LoadAttempt
rollbackUnresolvedAutoRefact targetsPlan rollbackState failedAttempt
  | Map.null rollbackState =
      pure failedAttempt
  | otherwise = do
      Log.info "Auto-refact: rolling back unresolved edits."
      rollbackAutoRefactorEdits rollbackState
      invalidateSymbolsMapCache
      invalidateModSummaries
      invalidateNameToInstancesIndex
      loadTargetsOnce targetsPlan

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
  Log.debug $ "GHC load completed with the following diagnostics:\n" <> intercalate "\n" (map show diagnostics)
  pure
    LoadAttempt
      { loadAttemptDiagnostics = dependencyDiagnostics <> diagnostics,
        loadAttemptResult = r,
        loadAttemptAutoRefactFiles = Set.empty
      }

countLoadedModules :: (MonadLore m) => Set.Set GHC.ModuleName -> m Int
countLoadedModules targetModules = do
  ModSummaries modSummaries <- getModSummaries
  let targetMods =
        [ mod'
        | mod' <- Map.keys modSummaries,
          GHC.moduleName mod' `Set.member` targetModules
        ]
  length <$> mapMaybeM GHC.getModuleInfo targetMods

rollbackStateFiles :: Map.Map FilePath a -> Set.Set FilePath
rollbackStateFiles =
  Set.fromList . Map.keys

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
