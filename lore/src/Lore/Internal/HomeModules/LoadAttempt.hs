{-# LANGUAGE CPP #-}

module Lore.Internal.HomeModules.LoadAttempt
  ( HomeModulesLoadAttempt (..),
    loadHomeModulesOnce,
    collectLoadedModules,
    countLoadedHomeModules,
    logDiagnosticsSummary,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Driver.Make as GHC
#if MIN_VERSION_ghc(9,8,0)
import qualified GHC.Types.Error as GHC.Error
#endif
import GHC.Utils.Monad (mapMaybeM)
import Lore.Diagnostics (Diagnostic (..), driverMessagesToDiagnostics, withDiagnosticsCapturing)
import Lore.Internal.HomeModules.ModuleGraphPatch (applyModuleScopedArgs)
import Lore.Internal.HomeModules.Plan (HomeModulesLoadPlan (..))
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.CacheInvalidation (retainCachesForLoadedModules)
import Lore.Internal.SourcePath (normalizeSourceFilePathM)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data HomeModulesLoadAttempt = HomeModulesLoadAttempt
  { homeModulesLoadAttemptDiagnostics :: [Diagnostic],
    homeModulesLoadAttemptResult :: GHC.SuccessFlag,
    homeModulesLoadAttemptModuleSummariesByFile :: Map.Map FilePath GHC.ModSummary,
    homeModulesLoadAttemptAutoRefactFiles :: Set.Set FilePath,
    homeModulesLoadAttemptAutoRefactSummaryByFile :: [(FilePath, [String])]
  }

{- ORMOLU_DISABLE -}
loadHomeModulesOnce :: (MonadLore m) => HomeModulesLoadPlan -> m HomeModulesLoadAttempt
loadHomeModulesOnce plan = do
  Log.debug "Starting dependency analysis and home-module loading..."
#if MIN_VERSION_ghc(9,14,0)
  (errs, modGraph) <- GHC.depanalE GHC.Error.mkUnknownDiagnostic Nothing [] False
#else
  (errs, modGraph) <- GHC.depanalE [] False
#endif
  let dependencyDiagnostics = driverMessagesToDiagnostics errs
  unless (null errs) $
    Log.err $
      "Errors during dependency analysis: "
        <> intercalate "\n" (map show dependencyDiagnostics)

  Log.debug "Patching module graph with component-specific GHC options..."
  patchedModGraph <- applyModuleScopedArgs plan.homeModulesComponentOptions modGraph
  moduleSummariesByFile <- buildModuleSummariesByFile patchedModGraph

  ifaceCacheVar <- asks ifaceCacheVar
  ifaceCache <- liftIO (MVar.readMVar ifaceCacheVar)
  Log.debug "Loading targets with GHC..."
  (diagnostics, loadResult) <- withDiagnosticsCapturing do
#if MIN_VERSION_ghc(9,8,0)
    GHC.load' (Just ifaceCache) GHC.LoadAllTargets GHC.Error.mkUnknownDiagnostic Nothing patchedModGraph
#else
    GHC.load' (Just ifaceCache) GHC.LoadAllTargets Nothing patchedModGraph
#endif

  loadedModules <- collectLoadedModules patchedModGraph
  retainCachesForLoadedModules loadedModules
  logDiagnosticsSummary diagnostics

  pure
    HomeModulesLoadAttempt
      { homeModulesLoadAttemptDiagnostics = dependencyDiagnostics <> diagnostics,
        homeModulesLoadAttemptResult = loadResult,
        homeModulesLoadAttemptModuleSummariesByFile = moduleSummariesByFile,
        homeModulesLoadAttemptAutoRefactFiles = Set.empty,
        homeModulesLoadAttemptAutoRefactSummaryByFile = []
      }
{- ORMOLU_ENABLE -}

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

countLoadedHomeModules :: (MonadLore m) => Set.Set GHC.ModuleName -> Set.Set FilePath -> m Int
countLoadedHomeModules namedHomeModules fileHomeModules = do
  normalizedFileHomeModules <- Set.fromList <$> mapM normalizeSourceFilePathM (Set.toList fileHomeModules)
  ModSummaries modSummaries <- getCachedModSummaries

  let moduleFromFileHomeModule (mod', summary) =
        case GHC.ml_hs_file (GHC.ms_location summary) of
          Nothing ->
            pure Nothing
          Just sourceFile -> do
            normalizedSourceFile <- normalizeSourceFilePathM sourceFile
            pure $
              if normalizedSourceFile `Set.member` normalizedFileHomeModules
                then Just mod'
                else Nothing

  targetModsFromFiles <- mapMaybeM moduleFromFileHomeModule (Map.toList modSummaries)

  let targetModsFromNames =
        [ mod'
        | mod' <- Map.keys modSummaries,
          GHC.moduleName mod' `Set.member` namedHomeModules
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
    then Log.debug "GHC load completed with no diagnostics."
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
