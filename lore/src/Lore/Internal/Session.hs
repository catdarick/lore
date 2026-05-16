module Lore.Internal.Session
  ( SessionContext (..),
    SessionConfig (..),
    defaultSessionConfig,
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
import Lore.Internal.File (defaultIgnoreList, findFilesByNameRecursively)
import Lore.Internal.Ghc.DynFlags (ParallelWorkersCount (..))
import Lore.Internal.Lookup.Cache.Types (ExternalSymbolsIndexCache (..), HomeSymbolsIndexCache (..), ModSummariesCache (..), NameToInstancesIndexCache (..), SymbolsDependencySetCache (..))
import Lore.Internal.PackageDB (resolvePackageDbPaths)
import Lore.Internal.Session.Cache.Types (InterpreterContextCache (..), LastLoadTargetsResultCache (..), TemporalModulesRegistry (..))
import Lore.Logger (LoggerHandle, noLogHandle)

data SessionContext = SessionContext
  { projectRoot :: FilePath,
    sessionGhcWorkDir :: FilePath,
    packageFiles :: [FilePath],
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
    packageDbPaths :: [FilePath],
    ifaceCache :: GHC.ModIfaceCache,
    homeSymbolsIndexCacheVar :: MVar HomeSymbolsIndexCache,
    externalSymbolsIndexCacheVar :: MVar ExternalSymbolsIndexCache,
    symbolsDependencySetCacheVar :: MVar SymbolsDependencySetCache,
    modSummariesCacheVar :: MVar ModSummariesCache,
    nameToInstancesIndexCacheVar :: MVar NameToInstancesIndexCache,
    parsedOccurrenceModuleIndexCacheVar :: MVar ParsedOccurrenceModuleIndexCache,
    definitionModuleIndexCacheVar :: MVar DefinitionModuleIndexCache,
    typedModuleFactsCacheVar :: MVar TypedModuleFactsCache,
    coreModuleFactsCacheVar :: MVar CoreModuleFactsCache,
    parsedModuleFactsCacheVar :: MVar ParsedModuleFactsCache,
    interpreterContextCacheVar :: MVar InterpreterContextCache,
    lastLoadTargetsResultCacheVar :: MVar LastLoadTargetsResultCache,
    temporalModulesRegistryVar :: MVar TemporalModulesRegistry
  }

data SessionConfig = SessionConfig
  { projectRoot :: FilePath,
    ghcWorkDir :: FilePath,
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
    parallelWorkersLimit :: ParallelWorkersCount
  }

defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { projectRoot = ".",
      ghcWorkDir = ".lore-work",
      loggerHandle = noLogHandle,
      customPrelude = Nothing,
      parallelWorkersLimit = WorkersAsNumProcessors
    }

prepareSessionContext :: SessionConfig -> IO (Either String SessionContext)
prepareSessionContext SessionConfig {projectRoot, ghcWorkDir = _ghcWorkDir, loggerHandle, customPrelude} = do
  packageFiles <- findFilesByNameRecursively (Just defaultIgnoreList) projectRoot "package.yaml"
  eiPackageDbPaths <- resolvePackageDbPaths projectRoot
  ifaceCache <- GHC.newIfaceCache
  homeSymbolsIndexCacheVar <- GHC.newMVar (HomeSymbolsIndexCache Nothing)
  externalSymbolsIndexCacheVar <- GHC.newMVar (ExternalSymbolsIndexCache Nothing)
  symbolsDependencySetCacheVar <- GHC.newMVar (SymbolsDependencySetCache Set.empty)
  modSummariesCacheVar <- GHC.newMVar (ModSummariesCache Nothing)
  nameToInstancesIndexCacheVar <- GHC.newMVar (NameToInstancesIndexCache Nothing)
  parsedOccurrenceModuleIndexCacheVar <- GHC.newMVar (ParsedOccurrenceModuleIndexCache Nothing)
  definitionModuleIndexCacheVar <- GHC.newMVar (DefinitionModuleIndexCache Map.empty)
  typedModuleFactsCacheVar <- GHC.newMVar (TypedModuleFactsCache Map.empty)
  coreModuleFactsCacheVar <- GHC.newMVar (CoreModuleFactsCache Map.empty)
  parsedModuleFactsCacheVar <- GHC.newMVar (ParsedModuleFactsCache Map.empty)
  interpreterContextCacheVar <- GHC.newMVar (InterpreterContextCache Nothing)
  lastLoadTargetsResultCacheVar <- GHC.newMVar (LastLoadTargetsResultCache Nothing)
  temporalModulesRegistryVar <- GHC.newMVar (TemporalModulesRegistry Nothing [])
  case eiPackageDbPaths of
    Left err -> pure $ Left $ "Failed to resolve package database paths: " <> err
    Right packageDbPaths -> do
      pure $
        Right
          SessionContext
            { projectRoot,
              sessionGhcWorkDir = _ghcWorkDir,
              packageFiles,
              loggerHandle,
              customPrelude,
              packageDbPaths = packageDbPaths,
              ifaceCache,
              homeSymbolsIndexCacheVar,
              externalSymbolsIndexCacheVar,
              symbolsDependencySetCacheVar,
              modSummariesCacheVar,
              nameToInstancesIndexCacheVar,
              parsedOccurrenceModuleIndexCacheVar,
              definitionModuleIndexCacheVar,
              typedModuleFactsCacheVar,
              coreModuleFactsCacheVar,
              parsedModuleFactsCacheVar,
              interpreterContextCacheVar,
              lastLoadTargetsResultCacheVar,
              temporalModulesRegistryVar
            }
