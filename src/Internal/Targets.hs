{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Move filter" #-}
module Internal.Targets where

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.Make as GHC
import qualified GHC.Driver.Session as GHC
import GHC.DynFlags (Extension (..), GhcOption (..), modifySessionDynFlagsM, setDependencies, setGhcOptionsAndExtensions, setGhcSourceDirs)
import qualified GHC.Parser.Header as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Unit.Module.Graph as GHC
import Internal.Diagnostics (driverMessagesToDiagnostics, withDiagnosticsCapturing)
import qualified Internal.Logger as Log
import Internal.Package (ComponentData (..), PackageData (..), defaultExtensions, extractDependencies, extractSourceDirs, prepareComponentsData)
import Monad (MonadLore)
import Session (SessionContext (..))

data TargetsPlan = TargetsPlan
  { commonExtensions :: Set.Set Extension,
    commonGhcOptions :: Set.Set GhcOption,
    modulesWithComponentOptions :: Map.Map GHC.ModuleName ComponentSpecificOptions
  }

data ComponentSpecificOptions = ComponentSpecificOptions
  { extensions :: Set.Set Extension,
    ghcOptions :: Set.Set GhcOption,
    baseDynFlags :: GHC.DynFlags
  }

prepareTargetsPlan :: (MonadLore m) => [ComponentData] -> m TargetsPlan
prepareTargetsPlan components = do
  sessionDynFlags <- GHC.getSessionDynFlags
  let commonExtensions = foldr1 Set.intersection (map defaultExtensions components)
      commonGhcOptions = foldr1 Set.intersection (map (.ghcOptions) components)

  modulesWithComponentOptions <- forM components \component -> do
    componentFlags <- setGhcOptionsAndExtensions (Set.toList component.ghcOptions) (Set.toList component.defaultExtensions) sessionDynFlags
    let componentSpecificExtensions = component.defaultExtensions Set.\\ commonExtensions
        componentSpecificGhcOptions = component.ghcOptions Set.\\ commonGhcOptions
        componentSpecificOptions =
          ComponentSpecificOptions
            { extensions = componentSpecificExtensions,
              ghcOptions = componentSpecificGhcOptions,
              baseDynFlags = componentFlags
            }
    pure $ Map.fromSet (const componentSpecificOptions) component.modules
  pure
    TargetsPlan
      { commonExtensions = commonExtensions,
        commonGhcOptions = commonGhcOptions,
        modulesWithComponentOptions = Map.unions modulesWithComponentOptions
      }

updateTargets :: (MonadLore m) => m ()
updateTargets = do
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
  Log.debug $ "Common GHC options: " <> show (Set.toList $ commonGhcOptions targetsPlan)
  Log.debug $ "Common extensions: " <> show (Set.toList $ commonExtensions targetsPlan)
  Log.debug $ "Dependencies to add: " <> show (Set.toList dependenciesToAdd)
  modifySessionDynFlagsM
    ( setGhcOptionsAndExtensions (Set.toList $ commonGhcOptions targetsPlan) (Set.toList $ commonExtensions targetsPlan)
        . setGhcSourceDirs (Set.toList sourceDirs)
        . setDependencies (Set.toList dependenciesToAdd)
    )
  let targets = map (mkModuleTarget homeUnitId) (Map.keys $ modulesWithComponentOptions targetsPlan)
  GHC.setTargets targets
  loadResult <- loadTargets targetsPlan
  case loadResult of
    GHC.Succeeded -> do
      Log.debug "Successfully updated GHC targets based on package.yaml configurations"
    GHC.Failed -> do
      Log.err "Failed to load GHC targets after updating. Please check the provided GHC options, source directories, and dependencies for correctness."

mkModuleTarget :: GHC.UnitId -> GHC.ModuleName -> GHC.Target
mkModuleTarget unitId modName =
  GHC.Target
    { GHC.targetId = GHC.TargetModule modName,
      GHC.targetAllowObjCode = True,
      GHC.targetUnitId = unitId,
      GHC.targetContents = Nothing
    }

loadTargets :: (MonadLore m) => TargetsPlan -> m GHC.SuccessFlag
loadTargets targetsPlan = do
  Log.debug "Starting dependency analysis and target loading..."
  (errs, modGraph) <- GHC.depanalE [] False
  unless (null errs) $ do
    let diagnostics = driverMessagesToDiagnostics errs
    Log.err $ "Errors during dependency analysis: " <> intercalate "\n" (map show diagnostics)
  Log.debug "Patching module graph with component-specific GHC options..."
  patchedModGraph <- applyModuleScopedArgs targetsPlan modGraph
  ifaceCache <- asks ifaceCache
  Log.debug "Loading targets with GHC..."
  (diagnostics, r) <- withDiagnosticsCapturing do
    GHC.load' (Just ifaceCache) GHC.LoadAllTargets Nothing patchedModGraph
  Log.debug $ "GHC load completed with the following diagnostics:\n" <> intercalate "\n" (map show diagnostics)
  pure r

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
            Just componentOptions | length componentOptions.ghcOptions + length componentOptions.extensions > 0 -> do
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
