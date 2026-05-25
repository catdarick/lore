module Lore.Internal.HomeModules.EntryModules
  ( ComponentEntryModule (..),
    collectLoadedComponentEntryModules,
    collectLoadedComponentEntryModulesWithDiagnostics,
    collectLoadedComponentModuleInfoWithDiagnostics,
    collectLoadedComponentModuleKinds,
    lookupGeneratedMainModulesByKey,
    resolveLoadedComponentEntryModule,
    resolveLoadedEntryModule,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.DeepSeq (NFData (..))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries, getCachedModSummariesByFile)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Package
  ( ComponentData (..),
    ComponentKind,
    PackageData (..),
    componentMainModulePathCandidates,
    firstExistingPath,
    prepareComponentsData,
  )
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (GeneratedMainModule (..), GeneratedMainModuleKey (..), GeneratedMainModulesRegistry (..))
import Lore.Internal.SourcePath (normalizeSourceFilePathM)
import Lore.Monad (MonadLore)
import System.FilePath (normalise, splitDirectories, (</>))

data ComponentEntryModule = ComponentEntryModule
  { entryPackageName :: String,
    entryComponentName :: String,
    entryComponentKind :: ComponentKind,
    entryOriginalMainPath :: FilePath,
    entryModule :: GHC.Module
  }

instance NFData ComponentEntryModule where
  rnf ComponentEntryModule {entryPackageName, entryComponentName, entryComponentKind, entryOriginalMainPath, entryModule} =
    rnf entryPackageName `seq`
      rnf entryComponentName `seq`
        rnf entryComponentKind `seq`
          rnf entryOriginalMainPath `seq`
            entryModule `seq`
              ()

collectLoadedComponentEntryModules :: (MonadLore m) => m [ComponentEntryModule]
collectLoadedComponentEntryModules =
  fst <$> collectLoadedComponentEntryModulesWithDiagnostics

collectLoadedComponentEntryModulesWithDiagnostics :: (MonadLore m) => m ([ComponentEntryModule], [String])
collectLoadedComponentEntryModulesWithDiagnostics = do
  (_, loadedEntryModules, entryResolutionErrors) <- collectLoadedComponentModuleInfoWithDiagnostics
  pure (loadedEntryModules, entryResolutionErrors)

collectLoadedComponentModuleInfoWithDiagnostics ::
  (MonadLore m) =>
  m (Map.Map GHC.Module (Set.Set ComponentKind), [ComponentEntryModule], [String])
collectLoadedComponentModuleInfoWithDiagnostics = do
  packages <- prepareComponentsData
  generatedMainModulesByKey <- lookupGeneratedMainModulesByKey
  modSummariesByFile <- getCachedModSummariesByFile
  ModSummaries modSummariesByModule <- getCachedModSummaries
  let rootedComponents =
        [ (pkg.packageName, pkg.packageRoot, component)
        | pkg <- packages,
          component <- pkg.components
        ]
  rootedComponentsWithSourceDirs <- forM rootedComponents \(packageName, packageRoot, component) -> do
    resolvedSourceDirs <- resolveComponentSourceDirs packageRoot component
    pure (packageName, packageRoot, component, resolvedSourceDirs)
  componentEntriesWithErrors <- forM rootedComponents \(packageName, packageRoot, component) ->
    resolveLoadedComponentEntryModule packageName packageRoot component generatedMainModulesByKey modSummariesByFile modSummariesByModule
  let entryResolutionErrors =
        [ "Failed to resolve entry module for "
            <> packageName
            <> "/"
            <> component.componentName
            <> ": "
            <> errorMessage
        | ((packageName, _, component), Left errorMessage) <- zip rootedComponents componentEntriesWithErrors
        ]
      loadedEntryModules =
        [ entryModule
        | Right entryModule <- componentEntriesWithErrors
        ]
      namedModulePairsByComponent =
        [ [ (module_, component.componentKind)
          | (module_, modSummary) <- Map.toList modSummariesByModule,
            let moduleName = GHC.moduleNameString (GHC.moduleName module_),
            Set.member (GHC.mkModuleName moduleName) component.modules,
            summaryBelongsToAnySourceDir modSummary resolvedSourceDirs
          ]
        | (_, _, component, resolvedSourceDirs) <- rootedComponentsWithSourceDirs
        ]
      entryPairsByComponent =
        [ (entry.entryModule, entry.entryComponentKind)
        | Right entry <- componentEntriesWithErrors
        ]
      moduleKindsByModule =
        Map.fromListWith Set.union $
          [ (module_, Set.singleton componentKind)
          | (module_, componentKind) <- concat namedModulePairsByComponent <> entryPairsByComponent
          ]
  pure (moduleKindsByModule, loadedEntryModules, entryResolutionErrors)

collectLoadedComponentModuleKinds :: (MonadLore m) => m (Map.Map GHC.Module (Set.Set ComponentKind))
collectLoadedComponentModuleKinds = do
  (moduleKindsByModule, _, _) <- collectLoadedComponentModuleInfoWithDiagnostics
  pure moduleKindsByModule

resolveLoadedComponentEntryModule ::
  (MonadLore m) =>
  String ->
  FilePath ->
  ComponentData ->
  Map.Map GeneratedMainModuleKey GeneratedMainModule ->
  Map.Map FilePath GHC.ModSummary ->
  Map.Map GHC.Module GHC.ModSummary ->
  m (Either String ComponentEntryModule)
resolveLoadedComponentEntryModule packageName packageRoot component generatedMainModulesByKey modSummariesByFile modSummariesByModule = do
  maybeMainPath <- firstExistingPath (componentMainModulePathCandidates packageRoot component)
  case maybeMainPath of
    Nothing ->
      pure (Left "main module path does not resolve to an existing file")
    Just mainPath -> do
      eiModule <- resolveLoadedEntryModule packageName component.componentName mainPath generatedMainModulesByKey modSummariesByFile modSummariesByModule
      pure $
        fmap
          ( \entryModule ->
              ComponentEntryModule
                { entryPackageName = packageName,
                  entryComponentName = component.componentName,
                  entryComponentKind = component.componentKind,
                  entryOriginalMainPath = mainPath,
                  entryModule
                }
          )
          eiModule

resolveLoadedEntryModule ::
  (MonadLore m) =>
  String ->
  String ->
  FilePath ->
  Map.Map GeneratedMainModuleKey GeneratedMainModule ->
  Map.Map FilePath GHC.ModSummary ->
  Map.Map GHC.Module GHC.ModSummary ->
  m (Either String GHC.Module)
resolveLoadedEntryModule packageName componentName mainPath generatedMainModulesByKey modSummariesByFile modSummariesByModule = do
  case Map.lookup generatedMainModuleKey generatedMainModulesByKey of
    Just generatedMainModule ->
      pure (resolveGeneratedEntryModule generatedMainModule.generatedMainModuleName modSummariesByModule)
    Nothing -> resolveNonSyntheticEntryModule mainPath modSummariesByFile
  where
    generatedMainModuleKey =
      GeneratedMainModuleKey
        { generatedMainPackageName = packageName,
          generatedMainComponentName = componentName,
          generatedMainOriginalPath = mainPath
        }

resolveGeneratedEntryModule ::
  String ->
  Map.Map GHC.Module GHC.ModSummary ->
  Either String GHC.Module
resolveGeneratedEntryModule generatedModuleName modSummariesByModule =
  case [ module_
       | module_ <- Map.keys modSummariesByModule,
         GHC.moduleNameString (GHC.moduleName module_) == generatedModuleName
       ] of
    [] ->
      Left ("entry module is not present in loaded module graph: " <> generatedModuleName)
    [module_] ->
      Right module_
    _ ->
      Left ("entry module is ambiguous in loaded module graph: " <> generatedModuleName)

resolveNonSyntheticEntryModule ::
  (MonadLore m) =>
  FilePath ->
  Map.Map FilePath GHC.ModSummary ->
  m (Either String GHC.Module)
resolveNonSyntheticEntryModule mainPath modSummariesByFile =
  do
    normalizedMainPath <- normalizeSourceFilePathM mainPath
    let candidatePaths =
          [ mainPath,
            normalise mainPath,
            normalizedMainPath,
            normalise normalizedMainPath
          ]
    pure $
      case firstMatchingSummary candidatePaths modSummariesByFile of
        Just modSummary ->
          Right (GHC.ms_mod modSummary)
        Nothing ->
          Left ("entry module is not present in loaded module graph: " <> mainPath)

firstMatchingSummary :: [FilePath] -> Map.Map FilePath GHC.ModSummary -> Maybe GHC.ModSummary
firstMatchingSummary candidatePaths modSummariesByFile =
  go candidatePaths
  where
    go [] = Nothing
    go (candidatePath : restPaths) =
      case Map.lookup candidatePath modSummariesByFile of
        Just summary -> Just summary
        Nothing -> go restPaths

lookupGeneratedMainModulesByKey :: (MonadLore m) => m (Map.Map GeneratedMainModuleKey GeneratedMainModule)
lookupGeneratedMainModulesByKey = do
  registryVar <- asks generatedMainModulesRegistryVar
  GeneratedMainModulesRegistry generatedMainModulesByKey <- liftIO (MVar.readMVar registryVar)
  pure generatedMainModulesByKey

resolveComponentSourceDirs :: (MonadLore m) => FilePath -> ComponentData -> m [FilePath]
resolveComponentSourceDirs packageRoot component = do
  normalizedSourceDirs <- mapM (normalizeSourceFilePathM . (packageRoot </>)) (Set.toList component.sourceDirs)
  pure (map normalise normalizedSourceDirs)

summaryBelongsToAnySourceDir :: GHC.ModSummary -> [FilePath] -> Bool
summaryBelongsToAnySourceDir modSummary sourceDirs =
  let sourcePath =
        normalise modSummary.ms_hspp_file
   in any (`isAncestorPath` sourcePath) sourceDirs

isAncestorPath :: FilePath -> FilePath -> Bool
isAncestorPath sourceDir sourcePath =
  splitDirectories sourceDir `List.isPrefixOf` splitDirectories sourcePath
