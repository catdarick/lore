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
    emptyParsedModuleFactsCache,
    emptyParsedOccurrenceModuleIndexCache,
    emptySimilarSymbolsSearchIndexCache,
    emptySymbolsDependencySetCache,
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
import Lore.Internal.Ghc.PackageEnvironment.Probe (captureGhcEnvironmentSnapshot)
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( GhcEnvironmentSnapshot,
  )
import Lore.Internal.Lookup.Cache.Types
  ( ExternalSymbolsIndexCache (..),
    HomeSymbolsIndexCache (..),
    ModSummariesCache (..),
    NameToInstancesIndexCache (..),
    SimilarSymbolsSearchIndexCache (..),
    SymbolsDependencySetCache (..),
  )
import Lore.Internal.Package.Discovery (discoverPackageRoots)
import Lore.Internal.Package.Materialize
  ( PackageMaterializeRunner,
    defaultPackageMaterializeRunnerFor,
    materializeCabalPackageFilesIO,
  )
import Lore.Internal.Package.Root (PackageRoot)
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
    isTestSuiteFunctionalityRequired :: Bool,
    projectProvider :: ProjectProvider,
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
    sessionPackageRoots :: [PackageRoot],
    sessionCabalPackageFiles :: [FilePath],
    ghcEnvironmentSnapshot :: GhcEnvironmentSnapshot,
    ifaceCache :: GHC.ModIfaceCache,
    homeSymbolsIndexCacheVar :: MVar HomeSymbolsIndexCache,
    externalSymbolsIndexCacheVar :: MVar ExternalSymbolsIndexCache,
    similarSymbolsSearchIndexCacheVar :: MVar SimilarSymbolsSearchIndexCache,
    symbolsDependencySetCacheVar :: MVar SymbolsDependencySetCache,
    modSummariesCacheVar :: MVar ModSummariesCache,
    nameToInstancesIndexCacheVar :: MVar NameToInstancesIndexCache,
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
    projectProviderOverride :: Maybe ProjectProvider,
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
    parallelWorkersLimit :: ParallelWorkersCount,
    isTestSuiteFunctionalityRequired :: Bool
  }

emptyHomeSymbolsIndexCache :: HomeSymbolsIndexCache
emptyHomeSymbolsIndexCache =
  HomeSymbolsIndexCache Nothing

emptyExternalSymbolsIndexCache :: ExternalSymbolsIndexCache
emptyExternalSymbolsIndexCache =
  ExternalSymbolsIndexCache Nothing

emptySimilarSymbolsSearchIndexCache :: SimilarSymbolsSearchIndexCache
emptySimilarSymbolsSearchIndexCache =
  SimilarSymbolsSearchIndexCache Nothing

emptySymbolsDependencySetCache :: SymbolsDependencySetCache
emptySymbolsDependencySetCache =
  SymbolsDependencySetCache Set.empty

emptyModSummariesCache :: ModSummariesCache
emptyModSummariesCache =
  ModSummariesCache Nothing

emptyNameToInstancesIndexCache :: NameToInstancesIndexCache
emptyNameToInstancesIndexCache =
  NameToInstancesIndexCache Nothing

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
prepareSessionContext SessionConfig {projectRoot, ghcWorkDir = _ghcWorkDir, projectProviderOverride, loggerHandle, customPrelude, isTestSuiteFunctionalityRequired} = do
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
        Right (sessionPackageRoots, sessionCabalPackageFiles) -> do
          eiGhcEnvironmentSnapshot <- captureGhcEnvironmentSnapshot projectProvider projectRoot
          case eiGhcEnvironmentSnapshot of
            Left err -> pure (Left err)
            Right ghcEnvironmentSnapshot -> do
              ifaceCache <- GHC.newIfaceCache
              homeSymbolsIndexCacheVar <- GHC.newMVar emptyHomeSymbolsIndexCache
              externalSymbolsIndexCacheVar <- GHC.newMVar emptyExternalSymbolsIndexCache
              similarSymbolsSearchIndexCacheVar <- GHC.newMVar emptySimilarSymbolsSearchIndexCache
              symbolsDependencySetCacheVar <- GHC.newMVar emptySymbolsDependencySetCache
              modSummariesCacheVar <- GHC.newMVar emptyModSummariesCache
              nameToInstancesIndexCacheVar <- GHC.newMVar emptyNameToInstancesIndexCache
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
                      isTestSuiteFunctionalityRequired,
                      projectProvider,
                      loggerHandle,
                      customPrelude,
                      sessionPackageRoots,
                      sessionCabalPackageFiles,
                      ghcEnvironmentSnapshot,
                      ifaceCache,
                      homeSymbolsIndexCacheVar,
                      externalSymbolsIndexCacheVar,
                      similarSymbolsSearchIndexCacheVar,
                      symbolsDependencySetCacheVar,
                      modSummariesCacheVar,
                      nameToInstancesIndexCacheVar,
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
