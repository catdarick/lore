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
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Types (ReferenceModuleAnalysis, ReferenceOccurrenceIndex)
import Lore.Internal.File (defaultIgnoreList, findFilesByNameRecursively)
import Lore.Internal.Ghc.DynFlags
  ( ParallelWorkersCount (..),
  )
import Lore.Internal.Lookup.Types (ExternalPackagesSymbolsCache, ModSummaries, NameToInstancesIndex, SymbolsIndex)
import Lore.Internal.PackageDB (resolvePackageDbPaths)
import Lore.Internal.Targets.Result (LoadTargetsResult)
import Lore.Logger (LoggerHandle, noLogHandle)

data SessionContext = SessionContext
  { projectRoot :: FilePath,
    packageFiles :: [FilePath],
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
    packageDbPaths :: [FilePath],
    ifaceCache :: GHC.ModIfaceCache,
    homeModulesSymbolsCache :: MVar (Maybe SymbolsIndex),
    externalPackagesSymbolsCache :: MVar (Maybe ExternalPackagesSymbolsCache),
    symbolsMapDependencySet :: MVar (Set.Set String),
    modSummariesCache :: MVar (Maybe ModSummaries),
    nameToInstancesIndexCache :: MVar (Maybe NameToInstancesIndex),
    referenceOccurrenceIndexCache :: MVar (Maybe ReferenceOccurrenceIndex),
    referenceModuleAnalysisCache :: MVar (Map.Map GHC.Module (Maybe ReferenceModuleAnalysis)),
    interpreterContextCache :: MVar (Maybe [GHC.ModuleName]),
    lastLoadTargetsResult :: MVar (Maybe LoadTargetsResult)
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
prepareSessionContext SessionConfig {projectRoot, loggerHandle, customPrelude} = do
  packageFiles <- findFilesByNameRecursively (Just defaultIgnoreList) projectRoot "package.yaml"
  eiPackageDbPaths <- resolvePackageDbPaths projectRoot
  ifaceCache <- GHC.newIfaceCache
  homeModulesSymbolsCache <- GHC.newMVar Nothing
  externalPackagesSymbolsCache <- GHC.newMVar Nothing
  symbolsMapDependencySet <- GHC.newMVar Set.empty
  modSummariesCache <- GHC.newMVar Nothing
  nameToInstancesIndexCache <- GHC.newMVar Nothing
  referenceOccurrenceIndexCache <- GHC.newMVar Nothing
  referenceModuleAnalysisCache <- GHC.newMVar Map.empty
  interpreterContextCache <- GHC.newMVar Nothing
  lastLoadTargetsResult <- GHC.newMVar Nothing
  case eiPackageDbPaths of
    Left err -> pure $ Left $ "Failed to resolve package database paths: " <> err
    Right packageDbPaths -> do
      pure $
        Right
          SessionContext
            { projectRoot,
              packageFiles,
              loggerHandle,
              customPrelude,
              packageDbPaths = packageDbPaths,
              ifaceCache,
              homeModulesSymbolsCache,
              externalPackagesSymbolsCache,
              symbolsMapDependencySet,
              modSummariesCache,
              nameToInstancesIndexCache,
              referenceOccurrenceIndexCache,
              referenceModuleAnalysisCache,
              interpreterContextCache,
              lastLoadTargetsResult
            }
