module Lore.Internal.Session
  ( SessionContext (..),
    SessionConfig (..),
    prepareSessionContext,
    ParallelWorkersCount (..),
  )
where

import qualified Control.Concurrent as GHC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC.Driver.Make as GHC
import GHC.MVar (MVar)
import Lore.Internal.Definition.Cache.Types (CoreModuleFactsCache (..), DefinitionModuleIndexCache (..), ParsedModuleFactsCache (..), ParsedOccurrenceModuleIndexCache (..), TypedModuleFactsCache (..))
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
import Lore.Internal.ProjectProvider
  ( ProjectProvider,
    detectProjectProvider,
  )
import Lore.Internal.Session.Cache.Types (GeneratedMainModulesRegistry (..), InterpreterContextCache (..), LastLoadHomeModulesResultCache (..), TemporalModulesRegistry (..))
import Lore.Logger (LoggerHandle)

data SessionContext = SessionContext
  { projectRoot :: FilePath,
    sessionGhcWorkDir :: FilePath,
    isTestSuiteFunctionalityRequired :: Bool,
    projectProvider :: ProjectProvider,
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
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

prepareSessionContext :: SessionConfig -> IO (Either String SessionContext)
prepareSessionContext SessionConfig {projectRoot, ghcWorkDir = _ghcWorkDir, projectProviderOverride, loggerHandle, customPrelude, isTestSuiteFunctionalityRequired} = do
  eiProvider <-
    case projectProviderOverride of
      Just provider -> pure (Right provider)
      Nothing -> detectProjectProvider projectRoot
  case eiProvider of
    Left err -> pure (Left err)
    Right projectProvider -> do
      eiGhcEnvironmentSnapshot <- captureGhcEnvironmentSnapshot projectProvider projectRoot
      case eiGhcEnvironmentSnapshot of
        Left err -> pure (Left err)
        Right ghcEnvironmentSnapshot -> do
          ifaceCache <- GHC.newIfaceCache
          homeSymbolsIndexCacheVar <- GHC.newMVar (HomeSymbolsIndexCache Nothing)
          externalSymbolsIndexCacheVar <- GHC.newMVar (ExternalSymbolsIndexCache Nothing)
          similarSymbolsSearchIndexCacheVar <- GHC.newMVar (SimilarSymbolsSearchIndexCache Nothing)
          symbolsDependencySetCacheVar <- GHC.newMVar (SymbolsDependencySetCache Set.empty)
          modSummariesCacheVar <- GHC.newMVar (ModSummariesCache Nothing)
          nameToInstancesIndexCacheVar <- GHC.newMVar (NameToInstancesIndexCache Nothing)
          parsedOccurrenceModuleIndexCacheVar <- GHC.newMVar (ParsedOccurrenceModuleIndexCache Nothing)
          definitionModuleIndexCacheVar <- GHC.newMVar (DefinitionModuleIndexCache Map.empty)
          typedModuleFactsCacheVar <- GHC.newMVar (TypedModuleFactsCache Map.empty)
          coreModuleFactsCacheVar <- GHC.newMVar (CoreModuleFactsCache Map.empty)
          parsedModuleFactsCacheVar <- GHC.newMVar (ParsedModuleFactsCache Map.empty)
          interpreterContextCacheVar <- GHC.newMVar (InterpreterContextCache Nothing)
          lastLoadHomeModulesResultCacheVar <- GHC.newMVar (LastLoadHomeModulesResultCache Nothing)
          generatedMainModulesRegistryVar <- GHC.newMVar (GeneratedMainModulesRegistry Map.empty)
          temporalModulesRegistryVar <- GHC.newMVar (TemporalModulesRegistry Nothing [])
          pure $
            Right
              SessionContext
                { projectRoot,
                  sessionGhcWorkDir = _ghcWorkDir,
                  isTestSuiteFunctionalityRequired,
                  projectProvider,
                  loggerHandle,
                  customPrelude,
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
