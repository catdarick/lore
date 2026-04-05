module Session where

import qualified Control.Concurrent as GHC
import qualified GHC.Driver.Make as GHC
import GHC.DynFlags (ParallelWorkersCount (..))
import GHC.MVar (MVar)
import Internal.File (defaultIgnoreList, findFilesByNameRecursively)
import Internal.Logger (LoggerHandle, loggerHandle'Pretty)
import Internal.Lookup.Types (SymbolsMap)
import Internal.PackageDB (resolvePackageDbPaths)

data SessionContext = SessionContext
  { projectRoot :: FilePath,
    packageFiles :: [FilePath],
    loggerHandle :: LoggerHandle,
    packageDbPaths :: [FilePath],
    ifaceCache :: GHC.ModIfaceCache,
    externalPackagesSymbolsCache :: MVar (Maybe SymbolsMap)
  }

data SessionConfig = SessionConfig
  { projectRoot :: FilePath,
    ghcWorkDir :: FilePath,
    loggerHandle :: LoggerHandle,
    parallelWorkersLimit :: ParallelWorkersCount
  }

defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { projectRoot = ".",
      ghcWorkDir = ".lore-work",
      loggerHandle = loggerHandle'Pretty,
      parallelWorkersLimit = WorkersAsNumProcessors
    }

prepareSessionContext :: SessionConfig -> IO (Either String SessionContext)
prepareSessionContext SessionConfig {projectRoot, loggerHandle} = do
  packageFiles <- findFilesByNameRecursively (Just defaultIgnoreList) projectRoot "package.yaml"
  eiPackageDbPaths <- resolvePackageDbPaths projectRoot
  ifaceCache <- GHC.newIfaceCache
  externalPackagesSymbolsCache <- GHC.newMVar Nothing
  case eiPackageDbPaths of
    Left err -> pure $ Left $ "Failed to resolve package database paths: " <> err
    Right packageDbPaths -> do
      pure $
        Right
          SessionContext
            { projectRoot,
              packageFiles,
              loggerHandle,
              packageDbPaths = packageDbPaths,
              ifaceCache,
              externalPackagesSymbolsCache
            }
