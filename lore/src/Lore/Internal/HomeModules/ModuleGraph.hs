{-# LANGUAGE CPP #-}

module Lore.Internal.HomeModules.ModuleGraph
  ( PreparedHomeModuleGraph (..),
    preparePatchedHomeModuleGraph,
    buildModuleSummariesByFile,
  )
where

import Control.Monad (unless)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified GHC
#if MIN_VERSION_ghc(9,8,0)
import qualified GHC.Types.Error as GHC.Error
#endif
import GHC.Utils.Monad (mapMaybeM)
import Lore.Diagnostics (Diagnostic, driverMessagesToDiagnostics)
import Lore.Internal.HomeModules.ModuleGraphPatch (applyModuleScopedArgs)
import Lore.Internal.HomeModules.Plan (HomeModulesLoadPlan (..))
import Lore.Internal.SourcePath (normalizeSourceFilePathM)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data PreparedHomeModuleGraph = PreparedHomeModuleGraph
  { preparedHomeModuleGraphDiagnostics :: [Diagnostic],
    preparedHomeModuleGraphModuleGraph :: GHC.ModuleGraph,
    preparedHomeModuleGraphSummariesByFile :: Map.Map FilePath GHC.ModSummary
  }

{- ORMOLU_DISABLE -}
preparePatchedHomeModuleGraph :: (MonadLore m) => HomeModulesLoadPlan -> m PreparedHomeModuleGraph
preparePatchedHomeModuleGraph plan = do
  Log.debug "Starting dependency analysis for home modules..."
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
  pure
    PreparedHomeModuleGraph
      { preparedHomeModuleGraphDiagnostics = dependencyDiagnostics,
        preparedHomeModuleGraphModuleGraph = patchedModGraph,
        preparedHomeModuleGraphSummariesByFile = moduleSummariesByFile
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
