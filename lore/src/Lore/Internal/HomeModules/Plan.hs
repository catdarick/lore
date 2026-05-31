module Lore.Internal.HomeModules.Plan
  ( HomeModulesLoadInputs (..),
    HomeModulesLoadConfig (..),
    HomeModulesSelection (..),
    HomeModulesLoadPlan (..),
    HomeModulesComponentPlan (..),
    HomeModuleKey (..),
    ComponentSpecificOptions (..),
    prepareHomeModulesLoadInputs,
    prepareHomeModulesLoadPlan,
    computeExternalHomeModuleDependencies,
    computeHomeModuleSourceDirs,
    buildHomeModulesSelection,
    homeModulesSelectionTotal,
    prepareHomeModulesComponentPlan,
    mkGhcModuleTarget,
    mkGhcFileTarget,
    commonComponentLanguage,
    commonSetIntersection,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.RWS (asks)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..), Language (..), setGhcOptionsAndExtensions)
import Lore.Internal.HomeModules.SyntheticMain (entryHomeModuleSource)
import Lore.Internal.Package
  ( ComponentData (..),
    PackageData (..),
    commonSetIntersection,
    componentMainModulePathCandidates,
    defaultExtensions,
    extractDependencies,
    extractSourceDirs,
    firstExistingPath,
    prepareComponentsData,
  )
import Lore.Internal.Ghc.PackageEnvironment.Resolve
  ( packageEnvironmentCacheKey,
    renderPackageResolutionError,
    resolveDependencyPackageEnvironment,
  )
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( GhcEnvironmentSnapshot,
    ResolvedPackageEnvironment,
  )
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.TemporalModules (TemporalModule (..), listExistingTemporalModules)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.FilePath (takeDirectory)
import UnliftIO.Exception (throwString)

data HomeModuleKey
  = HomeModuleName GHC.ModuleName
  | HomeModuleSourceFile FilePath
  deriving (Eq, Ord, Show)

data HomeModulesComponentPlan = HomeModulesComponentPlan
  { commonLanguage :: Maybe Language,
    commonExtensions :: Set.Set Extension,
    commonGhcOptions :: Set.Set GhcOption,
    homeModulesWithComponentOptions :: Map.Map HomeModuleKey ComponentSpecificOptions
  }

data ComponentSpecificOptions = ComponentSpecificOptions
  { language :: Maybe Language,
    extensions :: Set.Set Extension,
    ghcOptions :: Set.Set GhcOption,
    baseDynFlags :: GHC.DynFlags
  }

data HomeModulesLoadInputs = HomeModulesLoadInputs
  { homeModulesHomeUnitId :: GHC.UnitId,
    homeModulesGhcEnvironmentSnapshot :: GhcEnvironmentSnapshot,
    homeModulesPackages :: [PackageData],
    homeModulesTemporalModules :: [TemporalModule],
    homeModulesTestSuiteRequired :: Bool
  }

data HomeModulesLoadConfig = HomeModulesLoadConfig
  { homeModulesSourceDirs :: Set.Set FilePath,
    homeModulesDependencyNames :: Set.Set String,
    homeModulesPackageEnvironmentCacheKey :: Set.Set String,
    homeModulesPackageEnvironment :: ResolvedPackageEnvironment,
    homeModulesCommonLanguage :: Maybe Language,
    homeModulesCommonExtensions :: Set.Set Extension,
    homeModulesCommonGhcOptions :: Set.Set GhcOption
  }

data HomeModulesSelection = HomeModulesSelection
  { namedHomeModules :: Set.Set GHC.ModuleName,
    fileHomeModuleSources :: Set.Set FilePath,
    ghcTargets :: [GHC.Target]
  }

data HomeModulesLoadPlan = HomeModulesLoadPlan
  { homeModulesLoadConfig :: HomeModulesLoadConfig,
    homeModulesSelection :: HomeModulesSelection,
    homeModulesComponentOptions :: Map.Map HomeModuleKey ComponentSpecificOptions
  }

prepareHomeModulesLoadInputs :: (MonadLore m) => m HomeModulesLoadInputs
prepareHomeModulesLoadInputs = do
  dflags <- GHC.getSessionDynFlags
  ghcEnvironmentSnapshot <- asks ghcEnvironmentSnapshot
  testSuiteRequired <- asks isTestSuiteFunctionalityRequired
  packages <- prepareComponentsData
  temporalModules <- listExistingTemporalModules
  pure
    HomeModulesLoadInputs
      { homeModulesHomeUnitId = GHC.homeUnitId_ dflags,
        homeModulesGhcEnvironmentSnapshot = ghcEnvironmentSnapshot,
        homeModulesPackages = packages,
        homeModulesTemporalModules = temporalModules,
        homeModulesTestSuiteRequired = testSuiteRequired
      }

prepareHomeModulesLoadPlan :: (MonadLore m) => HomeModulesLoadInputs -> m HomeModulesLoadPlan
prepareHomeModulesLoadPlan inputs = do
  componentPlan <- prepareHomeModulesComponentPlan inputs.homeModulesPackages
  let dependencyNames =
        computeExternalHomeModuleDependencies
          inputs.homeModulesTestSuiteRequired
          inputs.homeModulesPackages
  packageEnvironment <-
    case
      resolveDependencyPackageEnvironment
        inputs.homeModulesGhcEnvironmentSnapshot
        dependencyNames of
      Left packageResolutionError ->
        throwString
          ( "Failed to resolve dependency package environment. "
              <> renderPackageResolutionError packageResolutionError
          )
      Right resolvedEnvironment ->
        pure resolvedEnvironment
  let environmentCacheKey = packageEnvironmentCacheKey packageEnvironment
      sourceDirs =
        computeHomeModuleSourceDirs
          inputs.homeModulesPackages
          inputs.homeModulesTemporalModules
      selection =
        buildHomeModulesSelection
          inputs.homeModulesHomeUnitId
          componentPlan
          inputs.homeModulesTemporalModules
  pure
    HomeModulesLoadPlan
      { homeModulesLoadConfig =
          HomeModulesLoadConfig
            { homeModulesSourceDirs = sourceDirs,
              homeModulesDependencyNames = dependencyNames,
              homeModulesPackageEnvironmentCacheKey = environmentCacheKey,
              homeModulesPackageEnvironment = packageEnvironment,
              homeModulesCommonLanguage = componentPlan.commonLanguage,
              homeModulesCommonExtensions = componentPlan.commonExtensions,
              homeModulesCommonGhcOptions = componentPlan.commonGhcOptions
            },
        homeModulesSelection = selection,
        homeModulesComponentOptions = componentPlan.homeModulesWithComponentOptions
      }

computeExternalHomeModuleDependencies :: Bool -> [PackageData] -> Set.Set String
computeExternalHomeModuleDependencies testSuiteRequired packages =
  runtimeDependencies Set.\\ localPackageNames
  where
    allComponents = concatMap (.components) packages
    localPackageNames = Set.fromList (map (.packageName) packages)
    dependencies = extractDependencies allComponents
    runtimeDependencies =
      if testSuiteRequired
        then Set.insert "directory" dependencies
        else dependencies

computeHomeModuleSourceDirs :: [PackageData] -> [TemporalModule] -> Set.Set FilePath
computeHomeModuleSourceDirs packages temporalModules =
  sourceDirs <> temporalSourceDirs
  where
    sourceDirs = Set.unions (map extractSourceDirs packages)
    temporalSourceDirs = Set.fromList (map (takeDirectory . modulePath) temporalModules)

buildHomeModulesSelection ::
  GHC.UnitId ->
  HomeModulesComponentPlan ->
  [TemporalModule] ->
  HomeModulesSelection
buildHomeModulesSelection homeUnitId componentPlan temporalModules =
  HomeModulesSelection
    { namedHomeModules = namedTargets,
      fileHomeModuleSources = fileTargets,
      ghcTargets =
        map (mkGhcModuleTarget homeUnitId) (Set.toList namedTargets)
          <> map (mkGhcFileTarget homeUnitId) (Set.toList fileTargets)
    }
  where
    homeModuleKeys = Map.keysSet componentPlan.homeModulesWithComponentOptions
    plannedNamedTargets =
      Set.fromList
        [ modName
        | HomeModuleName modName <- Set.toList homeModuleKeys
        ]
    fileTargets =
      Set.fromList
        [ sourcePath
        | HomeModuleSourceFile sourcePath <- Set.toList homeModuleKeys
        ]
    temporalNamedTargets = Set.fromList (map moduleName temporalModules)
    namedTargets = plannedNamedTargets <> temporalNamedTargets

homeModulesSelectionTotal :: HomeModulesSelection -> Int
homeModulesSelectionTotal selection =
  Set.size selection.namedHomeModules
    + Set.size selection.fileHomeModuleSources

prepareHomeModulesComponentPlan :: (MonadLore m) => [PackageData] -> m HomeModulesComponentPlan
prepareHomeModulesComponentPlan packages = do
  sessionDynFlags <- GHC.getSessionDynFlags
  let rootedComponents =
        [ (pkg.packageName, pkg.packageRoot, component)
        | pkg <- packages,
          component <- pkg.components
        ]
      components = map (\(_, _, component) -> component) rootedComponents
      commonLanguage = commonComponentLanguage components
      commonExtensions = commonSetIntersection (map defaultExtensions components)
      commonGhcOptions = commonSetIntersection (map (.ghcOptions) components)

  homeModulesWithComponentOptionsByComponent <- forM rootedComponents \(packageName, packageRoot, component) -> do
    componentFlags <-
      setGhcOptionsAndExtensions
        component.language
        (Set.toList component.ghcOptions)
        (Set.toList component.defaultExtensions)
        sessionDynFlags
    let componentSpecificExtensions = component.defaultExtensions Set.\\ commonExtensions
        componentSpecificGhcOptions = component.ghcOptions Set.\\ commonGhcOptions
        componentSpecificLanguage =
          if component.language == commonLanguage
            then Nothing
            else component.language
        componentSpecificOptions =
          ComponentSpecificOptions
            { language = componentSpecificLanguage,
              extensions = componentSpecificExtensions,
              ghcOptions = componentSpecificGhcOptions,
              baseDynFlags = componentFlags
            }
    componentHomeModules <- homeModuleKeysForComponent packageName packageRoot component
    pure (Map.fromSet (const componentSpecificOptions) componentHomeModules)

  homeModulesWithComponentOptions <- mergeHomeModuleComponentOptions homeModulesWithComponentOptionsByComponent

  pure
    HomeModulesComponentPlan
      { commonLanguage = commonLanguage,
        commonExtensions = commonExtensions,
        commonGhcOptions = commonGhcOptions,
        homeModulesWithComponentOptions = homeModulesWithComponentOptions
      }

commonComponentLanguage :: [ComponentData] -> Maybe Language
commonComponentLanguage [] = Nothing
commonComponentLanguage (component : restComponents)
  | all ((== component.language) . (.language)) restComponents = component.language
  | otherwise = Nothing

homeModuleKeysForComponent :: (MonadLore m) => String -> FilePath -> ComponentData -> m (Set.Set HomeModuleKey)
homeModuleKeysForComponent packageName packageRoot component = do
  maybeMainSourcePath <- firstExistingPath (componentMainModulePathCandidates packageRoot component)
  maybeEntryHomeModule <- mapM (entryHomeModuleSource packageName component.componentName) maybeMainSourcePath
  pure $
    Set.map HomeModuleName component.modules
      <> case maybeEntryHomeModule of
        Nothing -> Set.empty
        Just entrySourcePath -> Set.singleton (HomeModuleSourceFile entrySourcePath)

mkGhcModuleTarget :: GHC.UnitId -> GHC.ModuleName -> GHC.Target
mkGhcModuleTarget unitId modName =
  GHC.Target
    { GHC.targetId = GHC.TargetModule modName,
      GHC.targetAllowObjCode = True,
      GHC.targetUnitId = unitId,
      GHC.targetContents = Nothing
    }

mkGhcFileTarget :: GHC.UnitId -> FilePath -> GHC.Target
mkGhcFileTarget unitId sourceFile =
  GHC.Target
    { GHC.targetId = GHC.TargetFile sourceFile Nothing,
      GHC.targetAllowObjCode = True,
      GHC.targetUnitId = unitId,
      GHC.targetContents = Nothing
    }

mergeHomeModuleComponentOptions ::
  (MonadLore m) =>
  [Map.Map HomeModuleKey ComponentSpecificOptions] ->
  m (Map.Map HomeModuleKey ComponentSpecificOptions)
mergeHomeModuleComponentOptions =
  foldM mergeComponentMap Map.empty
  where
    mergeComponentMap merged componentMap =
      foldM mergeHomeModuleOptions merged (Map.toList componentMap)

    mergeHomeModuleOptions merged (homeModuleKey, newOptions) =
      case Map.lookup homeModuleKey merged of
        Nothing ->
          pure (Map.insert homeModuleKey newOptions merged)
        Just existingOptions
          | componentOptionsEquivalent existingOptions newOptions ->
              pure merged
          | otherwise -> do
              Log.warn $
                "Home-module planning conflict for "
                  <> renderHomeModuleKey homeModuleKey
                  <> ": component-specific options differ across components; keeping the first mapping."
              pure merged

renderHomeModuleKey :: HomeModuleKey -> String
renderHomeModuleKey homeModuleKey =
  case homeModuleKey of
    HomeModuleName moduleName ->
      "module " <> GHC.moduleNameString moduleName
    HomeModuleSourceFile sourcePath ->
      "source file " <> sourcePath

componentOptionsEquivalent :: ComponentSpecificOptions -> ComponentSpecificOptions -> Bool
componentOptionsEquivalent left right =
  left.language == right.language
    && left.extensions == right.extensions
    && left.ghcOptions == right.ghcOptions
