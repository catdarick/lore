module Lore.Internal.Targets.LoadAttempt
  ( LoadAttempt (..),
    loadTargetsOnce,
    collectLoadedModules,
    countLoadedTargets,
    logDiagnosticsSummary,
    mkModuleTarget,
    mkFileTarget,
  )
where

import Control.Monad (unless)
import Control.Monad.RWS (asks)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Driver.Make as GHC
import qualified GHC.Plugins as GHC
import GHC.Utils.Monad (mapMaybeM)
import Lore.Diagnostics (Diagnostic (..), driverMessagesToDiagnostics, withDiagnosticsCapturing)
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.CacheInvalidation (retainCachesForLoadedModules)
import Lore.Internal.SourcePath (normalizeSourceFilePathM)
import Lore.Internal.Targets.ModuleGraphPatch (applyModuleScopedArgs)
import Lore.Internal.Targets.Plan (TargetsPlan)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data LoadAttempt = LoadAttempt
  { loadAttemptDiagnostics :: [Diagnostic],
    loadAttemptResult :: GHC.SuccessFlag,
    loadAttemptModuleSummariesByFile :: Map.Map FilePath GHC.ModSummary,
    loadAttemptAutoRefactFiles :: Set.Set FilePath,
    loadAttemptAutoRefactSummaryByFile :: [(FilePath, [String])]
  }

loadTargetsOnce :: (MonadLore m) => TargetsPlan -> m LoadAttempt
loadTargetsOnce targetsPlan = do
  Log.debug "Starting dependency analysis and target loading..."
  (errs, modGraph) <- GHC.depanalE [] False
  let dependencyDiagnostics = driverMessagesToDiagnostics errs
  unless (null errs) $
    Log.err $
      "Errors during dependency analysis: "
        <> intercalate "\n" (map show dependencyDiagnostics)
  Log.debug "Patching module graph with component-specific GHC options..."
  patchedModGraph <- applyModuleScopedArgs targetsPlan modGraph
  moduleSummariesByFile <- buildModuleSummariesByFile patchedModGraph
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
        loadAttemptModuleSummariesByFile = moduleSummariesByFile,
        loadAttemptAutoRefactFiles = Set.empty,
        loadAttemptAutoRefactSummaryByFile = []
      }

buildModuleSummariesByFile :: (MonadLore m) => GHC.ModuleGraph -> m (Map.Map FilePath GHC.ModSummary)
buildModuleSummariesByFile moduleGraph = do
  pairs <- mapMaybeM summaryPair (GHC.mgModSummaries moduleGraph)
  pure (Map.fromList pairs)
  where
    summaryPair summary =
      case GHC.ml_hs_file (GHC.ms_location summary) of
        Nothing ->
          pure Nothing
        Just sourceFile -> do
          normalizedFilePath <- normalizeSourceFilePathM sourceFile
          pure (Just (normalizedFilePath, summary))

collectLoadedModules :: (MonadLore m) => GHC.ModuleGraph -> m (Set.Set GHC.Module)
collectLoadedModules moduleGraph = do
  loaded <- mapMaybeM keepLoadedModule [GHC.ms_mod modSummary | modSummary <- GHC.mgModSummaries moduleGraph]
  pure (Set.fromList loaded)
  where
    keepLoadedModule mod' = do
      maybeModuleInfo <- GHC.getModuleInfo mod'
      pure $
        case maybeModuleInfo of
          Just _ -> Just mod'
          Nothing -> Nothing

countLoadedTargets :: (MonadLore m) => Set.Set GHC.ModuleName -> Set.Set FilePath -> m Int
countLoadedTargets targetModules targetSourceFiles = do
  normalizedTargetSourceFiles <- Set.fromList <$> mapM normalizeSourceFilePathM (Set.toList targetSourceFiles)
  ModSummaries modSummaries <- getCachedModSummaries
  let moduleFromTargetSourceFile (mod', summary) =
        case GHC.ml_hs_file (GHC.ms_location summary) of
          Nothing ->
            pure Nothing
          Just sourceFile -> do
            normalizedSourceFile <- normalizeSourceFilePathM sourceFile
            pure $
              if normalizedSourceFile `Set.member` normalizedTargetSourceFiles
                then Just mod'
                else Nothing
  targetModsFromFiles <- mapMaybeM moduleFromTargetSourceFile (Map.toList modSummaries)
  let targetModsFromNames =
        [ mod'
        | mod' <- Map.keys modSummaries,
          GHC.moduleName mod' `Set.member` targetModules
        ]
      targetMods =
        Set.toList $
          Set.fromList (targetModsFromNames <> targetModsFromFiles)
  length <$> mapMaybeM GHC.getModuleInfo targetMods

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

mkFileTarget :: GHC.UnitId -> FilePath -> GHC.Target
mkFileTarget unitId sourceFile =
  GHC.Target
    { GHC.targetId = GHC.TargetFile sourceFile Nothing,
      GHC.targetAllowObjCode = True,
      GHC.targetUnitId = unitId,
      GHC.targetContents = Nothing
    }
