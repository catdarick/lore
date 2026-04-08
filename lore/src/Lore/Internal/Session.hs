module Lore.Internal.Session
  ( SessionContext (..),
    SessionConfig (..),
    PreludeImportRule (..),
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
import Lore.Logger (LoggerHandle, prettyLoggerHandle)

data PreludeImportRule
  = NoPrelude
  | ImportBasePrelude
  | ImportCustomPrelude Text
  deriving (Eq, Show)

data SessionContext = SessionContext
  { projectRoot :: FilePath,
    packageFiles :: [FilePath],
    loggerHandle :: LoggerHandle,
    interpreterPreludeImportRule :: PreludeImportRule,
    packageDbPaths :: [FilePath],
    ifaceCache :: GHC.ModIfaceCache,
    externalPackagesSymbolsCache :: MVar (Maybe SymbolsMap),
    modSummariesCache :: MVar (Maybe ModSummaries),
    nameToInstancesIndexCache :: MVar (Maybe NameToInstancesIndex),
    interpreterContextCache :: MVar (Maybe [GHC.ModuleName])
  }

data SessionConfig = SessionConfig
  { projectRoot :: FilePath,
    ghcWorkDir :: FilePath,
    loggerHandle :: LoggerHandle,
    interpreterPreludeImportRule :: PreludeImportRule,
    parallelWorkersLimit :: ParallelWorkersCount
  }

defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { projectRoot = ".",
      ghcWorkDir = ".lore-work",
      loggerHandle = prettyLoggerHandle,
      interpreterPreludeImportRule = ImportBasePrelude,
      parallelWorkersLimit = WorkersAsNumProcessors
    }

prepareSessionContext :: SessionConfig -> IO (Either String SessionContext)
prepareSessionContext SessionConfig {projectRoot, loggerHandle, interpreterPreludeImportRule} = do
  packageFiles <- findFilesByNameRecursively (Just defaultIgnoreList) projectRoot "package.yaml"
  eiPackageDbPaths <- resolvePackageDbPaths projectRoot
  ifaceCache <- GHC.newIfaceCache
  externalPackagesSymbolsCache <- GHC.newMVar Nothing
  modSummariesCache <- GHC.newMVar Nothing
  nameToInstancesIndexCache <- GHC.newMVar Nothing
  interpreterContextCache <- GHC.newMVar Nothing
  case eiPackageDbPaths of
    Left err -> pure $ Left $ "Failed to resolve package database paths: " <> err
    Right packageDbPaths -> do
      pure $
        Right
          SessionContext
            { projectRoot,
              packageFiles,
              loggerHandle,
              interpreterPreludeImportRule,
              packageDbPaths = packageDbPaths,
              ifaceCache,
              externalPackagesSymbolsCache,
              modSummariesCache,
              nameToInstancesIndexCache,
              interpreterContextCache
            }
