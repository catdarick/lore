module Lore.Internal.Session
  ( SessionContext (..),
    SessionConfig (..),
    defaultSessionConfig,
    prepareSessionContext,
    ParallelWorkersCount (..),
  )
where

import qualified Control.Concurrent as GHC
import Data.Text (Text)
import qualified GHC.Driver.Make as GHC
import GHC.MVar (MVar)
import qualified GHC.Plugins as GHC
import Lore.Internal.File (defaultIgnoreList, findFilesByNameRecursively)
import Lore.Internal.Ghc.DynFlags
  ( ParallelWorkersCount (..),
  )
import Lore.Internal.Lookup.Types (ModSummaries, NameToInstancesIndex, SymbolsMap)
import Lore.Internal.PackageDB (resolvePackageDbPaths)
import Lore.Internal.Targets.Result (LoadTargetsResult)
import Lore.Logger (LoggerHandle, prettyLoggerHandle)

data SessionContext = SessionContext
  { projectRoot :: FilePath,
    packageFiles :: [FilePath],
    loggerHandle :: LoggerHandle,
    customPrelude :: Maybe Text,
    packageDbPaths :: [FilePath],
    ifaceCache :: GHC.ModIfaceCache,
    externalPackagesSymbolsCache :: MVar (Maybe SymbolsMap),
    modSummariesCache :: MVar (Maybe ModSummaries),
    nameToInstancesIndexCache :: MVar (Maybe NameToInstancesIndex),
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
      loggerHandle = prettyLoggerHandle,
      customPrelude = Nothing,
      parallelWorkersLimit = WorkersAsNumProcessors
    }

prepareSessionContext :: SessionConfig -> IO (Either String SessionContext)
prepareSessionContext SessionConfig {projectRoot, loggerHandle, customPrelude} = do
  packageFiles <- findFilesByNameRecursively (Just defaultIgnoreList) projectRoot "package.yaml"
  eiPackageDbPaths <- resolvePackageDbPaths projectRoot
  ifaceCache <- GHC.newIfaceCache
  externalPackagesSymbolsCache <- GHC.newMVar Nothing
  modSummariesCache <- GHC.newMVar Nothing
  nameToInstancesIndexCache <- GHC.newMVar Nothing
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
              externalPackagesSymbolsCache,
              modSummariesCache,
              nameToInstancesIndexCache,
              interpreterContextCache,
              lastLoadTargetsResult
            }
