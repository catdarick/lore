module Lore.Session
  ( SessionContext (..),
    SessionConfig (..),
    SessionConfigError,
    ProjectProvider (..),
    ResolvedStartupConfig (..),
    defaultSessionConfig,
    loadStartupConfig,
    renderSessionConfigError,
    prepareSessionContext,
    runLore,
    ParallelWorkersCount (..),
    CacheMemorySnapshot (..),
    CacheMemoryStats (..),
    CacheMemoryDebugResult (..),
    debugSessionCachesMemory,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad ((<=<))
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import qualified GHC
import qualified GHC.Stats as RTS
import qualified GHC.Utils.Exception as GHCException
import Lore.Config (LoadedConfigDocument (..), loadConfigDocumentAt, loreConfigFileName)
import Lore.Internal.Definition.Callbacks (installDefinitionCallbacks)
import Lore.Internal.Ghc.DynFlags
  ( ParallelWorkersCount (..),
    modifySessionDynFlagsM,
    setGhcWorkDirs,
    setGhciLikeDynFlags,
    setPackageEnvironmentM,
  )
import Lore.Internal.Ghc.PackageEnvironment.Types (GhcToolchain (..))
import Lore.Internal.Monad (LoreMonadT (..), MonadLore)
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import Lore.Internal.Session
  ( SessionConfig (..),
    SessionContext (..),
    prepareSessionContext,
  )
import Lore.Internal.Session.Cache.Registry (SessionCacheResetAction (..), sessionCacheResetActions)
import Lore.Internal.Session.Environment
  ( SessionConfigError,
    SessionConfigOverrides (..),
    applySessionConfigOverrides,
    loadSessionEnvironmentOverrides,
    normalizePathRelativeTo,
    normalizeSessionConfigPaths,
    parseSessionConfigOverrides,
    renderSessionConfigError,
  )
import Lore.Logger (noLogHandle)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, makeAbsolute, setCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Mem (performMajorGC)

data CacheMemoryStats = CacheMemoryStats
  { cacheMemoryStatsName :: T.Text,
    cacheMemoryStatsBeforeLiveBytes :: Word64,
    cacheMemoryStatsAfterLiveBytes :: Word64,
    cacheMemoryStatsLiveBytesDelta :: Integer,
    cacheMemoryStatsBeforeMemInUseBytes :: Word64,
    cacheMemoryStatsAfterMemInUseBytes :: Word64,
    cacheMemoryStatsMemInUseBytesDelta :: Integer,
    cacheMemoryStatsBeforeMajorGcs :: Word32,
    cacheMemoryStatsAfterMajorGcs :: Word32
  }
  deriving stock (Eq, Show)

data CacheMemorySnapshot = CacheMemorySnapshot
  { cacheMemorySnapshotLiveBytes :: Word64,
    cacheMemorySnapshotMemInUseBytes :: Word64,
    cacheMemorySnapshotMajorGcs :: Word32
  }
  deriving stock (Eq, Show)

data CacheMemoryDebugResult = CacheMemoryDebugResult
  { cacheMemoryDebugRtsStatsEnabled :: Bool,
    cacheMemoryDebugGcRounds :: Int,
    cacheMemoryDebugGcDelayMicros :: Int,
    cacheMemoryDebugPreBaseline :: Maybe CacheMemorySnapshot,
    cacheMemoryDebugBaseline :: Maybe CacheMemorySnapshot,
    cacheMemoryDebugSamples :: [CacheMemoryStats]
  }
  deriving stock (Eq, Show)

defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { projectRoot = ".",
      ghcWorkDir = ".lore-work",
      configFilePath = loreConfigFileName,
      projectProviderOverride = Nothing,
      loggerHandle = noLogHandle,
      customPrelude = Nothing,
      parallelWorkersLimit = WorkersAsNumProcessors,
      testSuiteDefaultArguments = []
    }

data ResolvedStartupConfig = ResolvedStartupConfig
  { startupSessionConfig :: SessionConfig,
    startupConfigDocument :: LoadedConfigDocument
  }

loadStartupConfig :: IO (Either SessionConfigError ResolvedStartupConfig)
loadStartupConfig = do
  startupWorkingDirectory <- getCurrentDirectory
  eiEnvironmentOverrides <- loadSessionEnvironmentOverrides
  case eiEnvironmentOverrides of
    Left err ->
      pure (Left err)
    Right environmentOverrides -> do
      let bootstrapRoot =
            fromMaybe "." environmentOverrides.projectRootOverride
          absoluteBootstrapRoot =
            normalizePathRelativeTo startupWorkingDirectory bootstrapRoot
      absoluteConfigPath <- makeAbsolute (absoluteBootstrapRoot </> loreConfigFileName)
      eiDocument <- loadConfigDocumentAt absoluteConfigPath
      pure $
        case eiDocument of
          Left err ->
            Left err
          Right document ->
            case parseSessionConfigOverrides document of
              Left err ->
                Left err
              Right yamlOverrides ->
                let configDir = takeDirectory document.configFilePath
                    normalizedYamlOverrides =
                      yamlOverrides
                        { projectRootOverride =
                            normalizePathRelativeTo configDir <$> yamlOverrides.projectRootOverride
                        }
                    normalizedEnvironmentOverrides =
                      environmentOverrides
                        { projectRootOverride =
                            normalizePathRelativeTo startupWorkingDirectory <$> environmentOverrides.projectRootOverride
                        }
                    yamlConfig =
                      applySessionConfigOverrides normalizedYamlOverrides defaultSessionConfig
                    resolvedConfig =
                      applySessionConfigOverrides normalizedEnvironmentOverrides yamlConfig
                    normalizedConfig =
                      normalizeSessionConfigPaths document.configFilePath resolvedConfig
                 in Right
                      ResolvedStartupConfig
                        { startupSessionConfig = normalizedConfig,
                          startupConfigDocument = document
                        }

runLore :: (GHCException.ExceptionMonad m) => SessionConfig -> LoreMonadT m a -> m a
runLore sessionConfig lore = do
  eiSessionContext <- liftIO $ prepareSessionContext sessionConfig
  case eiSessionContext of
    Left err ->
      error err
    Right sessionContext@SessionContext {projectRoot = sessionProjectRoot} ->
      bracket
        ( liftIO do
            cwd <- getCurrentDirectory
            setCurrentDirectory sessionProjectRoot
            pure cwd
        )
        (liftIO . setCurrentDirectory)
        (\_ -> GHC.runGhcT (Just sessionContext.ghcToolchain.ghcToolchainLibDir) $ setupGhcSession sessionContext >> runReaderT (unLoreMonadT lore) sessionContext)
  where
    setupGhcSession sessionContext = do
      liftIO $ do
        let workDir = ghcWorkDir sessionConfig
        mapM_
          (createDirectoryIfMissing True)
          [ workDir,
            workDir </> "obj",
            workDir </> "hi",
            workDir </> "hie",
            workDir </> "stub",
            workDir </> "tmp"
          ]
      modifySessionDynFlagsM
        ( setPackageEnvironmentM sessionContext.startupPackageEnvironment
            <=< pure
              . setGhciLikeDynFlags (parallelWorkersLimit sessionConfig)
              . setGhcWorkDirs (ghcWorkDir sessionConfig)
        )
      session <- GHC.getSession
      GHC.setSession (installDefinitionCallbacks sessionContext session)

debugSessionCachesMemory :: (MonadLore m) => m CacheMemoryDebugResult
debugSessionCachesMemory = do
  sessionContext <- asks id
  statsEnabled <- liftIO RTS.getRTSStatsEnabled
  if not statsEnabled
    then
      pure
        CacheMemoryDebugResult
          { cacheMemoryDebugRtsStatsEnabled = False,
            cacheMemoryDebugGcRounds = cacheMemoryDebugMajorGcRounds,
            cacheMemoryDebugGcDelayMicros = cacheMemoryDebugGcDelayMicrosConst,
            cacheMemoryDebugPreBaseline = Nothing,
            cacheMemoryDebugBaseline = Nothing,
            cacheMemoryDebugSamples = []
          }
    else do
      preBaseline <- liftIO readCacheMemorySnapshot
      liftIO runMajorGcCycles
      baseline <- liftIO readCacheMemorySnapshot
      samples <- liftIO (measureCacheMemoryImpacts sessionContext baseline sessionCacheResetActions)
      pure
        CacheMemoryDebugResult
          { cacheMemoryDebugRtsStatsEnabled = True,
            cacheMemoryDebugGcRounds = cacheMemoryDebugMajorGcRounds,
            cacheMemoryDebugGcDelayMicros = cacheMemoryDebugGcDelayMicrosConst,
            cacheMemoryDebugPreBaseline = Just preBaseline,
            cacheMemoryDebugBaseline = Just baseline,
            cacheMemoryDebugSamples = samples
          }

cacheMemoryDebugMajorGcRounds :: Int
cacheMemoryDebugMajorGcRounds = 10

cacheMemoryDebugGcDelayMicrosConst :: Int
cacheMemoryDebugGcDelayMicrosConst = 300_000

measureCacheMemoryImpacts :: SessionContext -> CacheMemorySnapshot -> [SessionCacheResetAction] -> IO [CacheMemoryStats]
measureCacheMemoryImpacts _ _ [] =
  pure []
measureCacheMemoryImpacts sessionContext baseline (firstAction : remainingActions) = do
  firstSample <- measureCacheMemoryImpactFrom sessionContext baseline firstAction
  remainingSamples <- mapM (measureCacheMemoryImpact sessionContext) remainingActions
  pure (firstSample : remainingSamples)

measureCacheMemoryImpact :: SessionContext -> SessionCacheResetAction -> IO CacheMemoryStats
measureCacheMemoryImpact sessionContext resetAction = do
  before <- readCacheMemorySnapshot
  measureCacheMemoryImpactFrom sessionContext before resetAction

measureCacheMemoryImpactFrom :: SessionContext -> CacheMemorySnapshot -> SessionCacheResetAction -> IO CacheMemoryStats
measureCacheMemoryImpactFrom sessionContext before resetAction = do
  sessionCacheResetActionRun resetAction sessionContext.sessionCacheVars
  runMajorGcCycles
  after <- readCacheMemorySnapshot
  pure
    CacheMemoryStats
      { cacheMemoryStatsName = resetAction.sessionCacheResetActionName,
        cacheMemoryStatsBeforeLiveBytes = before.cacheMemorySnapshotLiveBytes,
        cacheMemoryStatsAfterLiveBytes = after.cacheMemorySnapshotLiveBytes,
        cacheMemoryStatsLiveBytesDelta = integerDelta before.cacheMemorySnapshotLiveBytes after.cacheMemorySnapshotLiveBytes,
        cacheMemoryStatsBeforeMemInUseBytes = before.cacheMemorySnapshotMemInUseBytes,
        cacheMemoryStatsAfterMemInUseBytes = after.cacheMemorySnapshotMemInUseBytes,
        cacheMemoryStatsMemInUseBytesDelta = integerDelta before.cacheMemorySnapshotMemInUseBytes after.cacheMemorySnapshotMemInUseBytes,
        cacheMemoryStatsBeforeMajorGcs = before.cacheMemorySnapshotMajorGcs,
        cacheMemoryStatsAfterMajorGcs = after.cacheMemorySnapshotMajorGcs
      }

runMajorGcCycles :: IO ()
runMajorGcCycles =
  mapM_
    (\_ -> performMajorGC >> threadDelay cacheMemoryDebugGcDelayMicrosConst)
    [1 .. cacheMemoryDebugMajorGcRounds]

readCacheMemorySnapshot :: IO CacheMemorySnapshot
readCacheMemorySnapshot = do
  rtsStats <- RTS.getRTSStats
  let gcDetails = RTS.gc rtsStats
  pure
    CacheMemorySnapshot
      { cacheMemorySnapshotLiveBytes = RTS.gcdetails_live_bytes gcDetails,
        cacheMemorySnapshotMemInUseBytes = RTS.gcdetails_mem_in_use_bytes gcDetails,
        cacheMemorySnapshotMajorGcs = RTS.major_gcs rtsStats
      }

integerDelta :: Word64 -> Word64 -> Integer
integerDelta before after =
  toInteger after - toInteger before
