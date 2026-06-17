module Lore.Internal.Session
  ( SessionContext (..),
    SessionConfig (..),
    emptyCoreModuleFactsCache,
    emptyDefinitionModuleIndexCache,
    emptyExternalSymbolsIndexCache,
    emptyGeneratedMainModulesRegistry,
    emptyHomeSymbolsIndexCache,
    emptyInterpreterContextCache,
    emptyLastLoadHomeModulesResultCache,
    emptyModSummariesCache,
    emptyNameToInstancesIndexCache,
    emptyInstanceEnvironmentInputsCache,
    emptyParsedModuleFactsCache,
    emptyParsedOccurrenceModuleIndexCache,
    emptySymbolSearchIndexCache,
    emptyExternalSymbolsEnvironmentKeyCache,
    emptyTemporalModulesRegistry,
    emptyTypedModuleFactsCache,
    preparePackageMaterializationBeforeEnvironmentProbe,
    preparePackageMaterializationBeforeEnvironmentProbeWithRunner,
    prepareSessionContext,
    ParallelWorkersCount (..),
  )
where

import qualified Control.Concurrent as GHC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified GHC.Driver.Make as GHC
import GHC.MVar (MVar)
import Lore.Internal.Definition.Cache.Types (CoreModuleFactsCache, DefinitionModuleIndexCache (..), ModuleCache (..), ParsedModuleFactsCache, ParsedOccurrenceModuleIndexCache (..), TypedModuleFactsCache)
import Lore.Internal.Ghc.DynFlags (ParallelWorkersCount (..))
import Lore.Internal.Ghc.PackageEnvironment.Probe (captureGhcEnvironment)
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( CapturedGhcEnvironment (..),
    GhcToolchain,
    PackageEnvironmentSnapshot (..),
    ResolvedPackageEnvironment (..),
  )
import Lore.Internal.Lookup.Cache.Types
  ( ExternalSymbolsEnvironmentKeyCache (..),
    ExternalSymbolsIndexCache (..),
    HomeSymbolsIndexCache (..),
    InstanceEnvironmentInputsCache (..),
    ModSummariesCache (..),
    NameToInstancesIndexCache (..),
    SymbolSearchIndexCache (..),
  )
import Lore.Internal.Package.Discovery (discoverPackageRoots)
import Lore.Internal.Package.Materialize
  ( PackageMaterializeRunner,
    defaultPackageMaterializeRunnerFor,
    materializeCabalPackageFilesIO,
  )
import Lore.Internal.Package.Root (PackageRoot)
import Lore.Internal.ProjectEnvironment.Types (ProjectEnvironmentState)
import Lore.Internal.ProjectProvider
  ( ProjectProvider,
    detectProjectProvider,
  )
import Lore.Internal.Session.Cache.Types (GeneratedMainModulesRegistry (..), InterpreterContextCache (..), LastLoadHomeModulesResultCache (..), TemporalModulesRegistry (..))
import Lore.Internal.SourceText (relativeSourcePath)
import Lore.Logger (LogLevel (..), LogMessage (..), LoggerHandle (..))

data SessionContext = SessionContext
  { projectRoot :: FilePath,
    sessionGhcWorkDir :: FilePath,
    configFilePath :: FilePath,
    isTestSuiteFunctionalityRequired :: Bool,
    projectProvider :: ProjectProvider,
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
    testSuiteDefaultArguments :: [String],
    ghcToolchain :: GhcToolchain,
    startupPackageEnvironment :: ResolvedPackageEnvironment,
    projectEnvironmentStateVar :: MVar (Maybe ProjectEnvironmentState),
    ifaceCacheVar :: MVar GHC.ModIfaceCache,
    homeSymbolsIndexCacheVar :: MVar HomeSymbolsIndexCache,
    externalSymbolsIndexCacheVar :: MVar ExternalSymbolsIndexCache,
    symbolSearchIndexCacheVar :: MVar SymbolSearchIndexCache,
    externalSymbolsEnvironmentKeyCacheVar :: MVar ExternalSymbolsEnvironmentKeyCache,
    modSummariesCacheVar :: MVar ModSummariesCache,
    nameToInstancesIndexCacheVar :: MVar NameToInstancesIndexCache,
    instanceEnvironmentInputsCacheVar :: MVar InstanceEnvironmentInputsCache,
    parsedOccurrenceModuleIndexCacheVar :: MVar ParsedOccurrenceModuleIndexCache,
    definitionModuleIndexCacheVar :: MVar DefinitionModuleIndexCache,
    typedModuleFactsCacheVar :: MVar TypedModuleFactsCache,
    coreModuleFactsCacheVar :: MVar CoreModuleFactsCache,
    parsedModuleFactsCacheVar :: MVar ParsedModuleFactsCache,
    interpreterContextCacheVar :: MVar InterpreterContextCache,
    lastLoadHomeModulesResultCacheVar :: MVar LastLoadHomeModulesResultCache,
    generatedMainModulesRegistryVar :: MVar GeneratedMainModulesRegistry,
    temporalModulesRegistryVar :: MVar TemporalModulesRegistry
  }

data SessionConfig = SessionConfig
  { projectRoot :: FilePath,
    ghcWorkDir :: FilePath,
    configFilePath :: FilePath,
    projectProviderOverride :: Maybe ProjectProvider,
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
    parallelWorkersLimit :: ParallelWorkersCount,
    testSuiteDefaultArguments :: [String],
    isTestSuiteFunctionalityRequired :: Bool
  }

emptyHomeSymbolsIndexCache :: HomeSymbolsIndexCache
emptyHomeSymbolsIndexCache =
  HomeSymbolsIndexCache Nothing

emptyExternalSymbolsIndexCache :: ExternalSymbolsIndexCache
emptyExternalSymbolsIndexCache =
  ExternalSymbolsIndexCache Nothing

emptySymbolSearchIndexCache :: SymbolSearchIndexCache
emptySymbolSearchIndexCache =
  SymbolSearchIndexCache Nothing

emptyExternalSymbolsEnvironmentKeyCache :: ExternalSymbolsEnvironmentKeyCache
emptyExternalSymbolsEnvironmentKeyCache =
  ExternalSymbolsEnvironmentKeyCache Set.empty

emptyModSummariesCache :: ModSummariesCache
emptyModSummariesCache =
  ModSummariesCache Nothing

emptyNameToInstancesIndexCache :: NameToInstancesIndexCache
emptyNameToInstancesIndexCache =
  NameToInstancesIndexCache Nothing

emptyInstanceEnvironmentInputsCache :: InstanceEnvironmentInputsCache
emptyInstanceEnvironmentInputsCache =
  InstanceEnvironmentInputsCache Nothing

emptyParsedOccurrenceModuleIndexCache :: ParsedOccurrenceModuleIndexCache
emptyParsedOccurrenceModuleIndexCache =
  ParsedOccurrenceModuleIndexCache Nothing

emptyDefinitionModuleIndexCache :: DefinitionModuleIndexCache
emptyDefinitionModuleIndexCache =
  DefinitionModuleIndexCache Map.empty

emptyTypedModuleFactsCache :: TypedModuleFactsCache
emptyTypedModuleFactsCache =
  ModuleCache Map.empty

emptyCoreModuleFactsCache :: CoreModuleFactsCache
emptyCoreModuleFactsCache =
  ModuleCache Map.empty

emptyParsedModuleFactsCache :: ParsedModuleFactsCache
emptyParsedModuleFactsCache =
  ModuleCache Map.empty

emptyInterpreterContextCache :: InterpreterContextCache
emptyInterpreterContextCache =
  InterpreterContextCache Nothing

emptyLastLoadHomeModulesResultCache :: LastLoadHomeModulesResultCache
emptyLastLoadHomeModulesResultCache =
  LastLoadHomeModulesResultCache Nothing

emptyGeneratedMainModulesRegistry :: GeneratedMainModulesRegistry
emptyGeneratedMainModulesRegistry =
  GeneratedMainModulesRegistry Map.empty

emptyTemporalModulesRegistry :: TemporalModulesRegistry
emptyTemporalModulesRegistry =
  TemporalModulesRegistry Nothing []

prepareSessionContext :: SessionConfig -> IO (Either String SessionContext)
prepareSessionContext SessionConfig {projectRoot, ghcWorkDir = _ghcWorkDir, configFilePath, projectProviderOverride, loggerHandle, customPrelude, testSuiteDefaultArguments, isTestSuiteFunctionalityRequired} = do
  eiProvider <-
    case projectProviderOverride of
      Just provider -> pure (Right provider)
      Nothing -> detectProjectProvider projectRoot
  case eiProvider of
    Left err -> pure (Left err)
    Right projectProvider -> do
      eiMaterializedPackages <-
        preparePackageMaterializationBeforeEnvironmentProbe loggerHandle projectProvider projectRoot
      case eiMaterializedPackages of
        Left err -> pure (Left err)
        Right _ -> do
          eiGhcEnvironment <- captureGhcEnvironment projectProvider projectRoot
          case eiGhcEnvironment of
            Left err -> pure (Left err)
            Right capturedEnvironment -> do
              let startupPackageEnvironment = resolvedEnvironmentFromSnapshot capturedEnvironment.capturedPackageEnvironment
              projectEnvironmentStateVar <- GHC.newMVar Nothing
              ifaceCache <- GHC.newIfaceCache
              ifaceCacheVar <- GHC.newMVar ifaceCache
              homeSymbolsIndexCacheVar <- GHC.newMVar emptyHomeSymbolsIndexCache
              externalSymbolsIndexCacheVar <- GHC.newMVar emptyExternalSymbolsIndexCache
              symbolSearchIndexCacheVar <- GHC.newMVar emptySymbolSearchIndexCache
              externalSymbolsEnvironmentKeyCacheVar <- GHC.newMVar emptyExternalSymbolsEnvironmentKeyCache
              modSummariesCacheVar <- GHC.newMVar emptyModSummariesCache
              nameToInstancesIndexCacheVar <- GHC.newMVar emptyNameToInstancesIndexCache
              instanceEnvironmentInputsCacheVar <- GHC.newMVar emptyInstanceEnvironmentInputsCache
              parsedOccurrenceModuleIndexCacheVar <- GHC.newMVar emptyParsedOccurrenceModuleIndexCache
              definitionModuleIndexCacheVar <- GHC.newMVar emptyDefinitionModuleIndexCache
              typedModuleFactsCacheVar <- GHC.newMVar emptyTypedModuleFactsCache
              coreModuleFactsCacheVar <- GHC.newMVar emptyCoreModuleFactsCache
              parsedModuleFactsCacheVar <- GHC.newMVar emptyParsedModuleFactsCache
              interpreterContextCacheVar <- GHC.newMVar emptyInterpreterContextCache
              lastLoadHomeModulesResultCacheVar <- GHC.newMVar emptyLastLoadHomeModulesResultCache
              generatedMainModulesRegistryVar <- GHC.newMVar emptyGeneratedMainModulesRegistry
              temporalModulesRegistryVar <- GHC.newMVar emptyTemporalModulesRegistry
              pure $
                Right
                  SessionContext
                    { projectRoot,
                      sessionGhcWorkDir = _ghcWorkDir,
                      configFilePath,
                      isTestSuiteFunctionalityRequired,
                      projectProvider,
                      loggerHandle,
                      customPrelude,
                      testSuiteDefaultArguments,
                      ghcToolchain = capturedEnvironment.capturedGhcToolchain,
                      startupPackageEnvironment,
                      projectEnvironmentStateVar,
                      ifaceCacheVar,
                      homeSymbolsIndexCacheVar,
                      externalSymbolsIndexCacheVar,
                      symbolSearchIndexCacheVar,
                      externalSymbolsEnvironmentKeyCacheVar,
                      modSummariesCacheVar,
                      nameToInstancesIndexCacheVar,
                      instanceEnvironmentInputsCacheVar,
                      parsedOccurrenceModuleIndexCacheVar,
                      definitionModuleIndexCacheVar,
                      typedModuleFactsCacheVar,
                      coreModuleFactsCacheVar,
                      parsedModuleFactsCacheVar,
                      interpreterContextCacheVar,
                      lastLoadHomeModulesResultCacheVar,
                      generatedMainModulesRegistryVar,
                      temporalModulesRegistryVar
                    }

preparePackageMaterializationBeforeEnvironmentProbe ::
  LoggerHandle ->
  ProjectProvider ->
  FilePath ->
  IO (Either String ([PackageRoot], [FilePath]))
preparePackageMaterializationBeforeEnvironmentProbe loggerHandle projectProvider =
  preparePackageMaterializationBeforeEnvironmentProbeWithRunner
    (defaultPackageMaterializeRunnerFor projectProvider)
    loggerHandle
    projectProvider

preparePackageMaterializationBeforeEnvironmentProbeWithRunner ::
  PackageMaterializeRunner ->
  LoggerHandle ->
  ProjectProvider ->
  FilePath ->
  IO (Either String ([PackageRoot], [FilePath]))
preparePackageMaterializationBeforeEnvironmentProbeWithRunner runner loggerHandle projectProvider projectRoot = do
  eiPackageRoots <- discoverPackageRoots projectProvider projectRoot
  case eiPackageRoots of
    Left err ->
      pure (Left err)
    Right packageRoots -> do
      eiCabalFiles <-
        materializeCabalPackageFilesIO
          runner
          logInfoWithHandle
          (relativeSourcePath projectRoot)
          packageRoots
      case eiCabalFiles of
        Left err ->
          pure (Left err)
        Right cabalFiles ->
          pure (Right (packageRoots, cabalFiles))
  where
    logInfoWithHandle message = do
      currentTime <- getCurrentTime
      loggerHandle.putLog
        LogMessage
          { timestamp = currentTime,
            level = Info,
            content = message
          }

resolvedEnvironmentFromSnapshot :: PackageEnvironmentSnapshot -> ResolvedPackageEnvironment
resolvedEnvironmentFromSnapshot snapshot =
  ResolvedPackageEnvironment
    { resolvedPackageDbStack = snapshot.packageEnvironmentPackageDbStack,
      resolvedExposedUnitIds = Set.unions (Map.elems snapshot.packageEnvironmentSelectedUnitIdsByPackageName)
    }
