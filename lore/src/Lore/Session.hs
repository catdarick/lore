module Lore.Session
  ( SessionContext (..),
    SessionConfig (..),
    ProjectProvider (..),
    defaultSessionConfig,
    prepareSessionContext,
    runLore,
    ParallelWorkersCount (..),
  )
where

import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Utils.Exception as GHCException
import Lore.Internal.Definition.Callbacks (installDefinitionCallbacks)
import Lore.Internal.Ghc.DynFlags
  ( ParallelWorkersCount (..),
    modifySessionDynFlagsM,
    setGhcWorkDirs,
    setGhciLikeDynFlags,
    setPackageEnvironmentM,
  )
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( GhcEnvironmentSnapshot (..),
    ResolvedPackageEnvironment (..),
  )
import Lore.Internal.Monad (LoreMonadT (..))
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import Lore.Internal.Session
  ( SessionConfig (..),
    SessionContext (..),
    prepareSessionContext,
  )
import Lore.Logger (noLogHandle)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))

defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { projectRoot = ".",
      ghcWorkDir = ".lore-work",
      projectProviderOverride = Nothing,
      loggerHandle = noLogHandle,
      customPrelude = Nothing,
      parallelWorkersLimit = WorkersAsNumProcessors,
      isTestSuiteFunctionalityRequired = False
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
        (\_ -> GHC.runGhcT (Just sessionContext.ghcEnvironmentSnapshot.ghcEnvironmentLibDir) $ setupGhcSession sessionContext >> runReaderT (unLoreMonadT lore) sessionContext)
  where
    setupGhcSession sessionContext = do
      let initialPackageEnvironment =
            ResolvedPackageEnvironment
              { resolvedPackageDbStack = sessionContext.ghcEnvironmentSnapshot.ghcEnvironmentPackageDbStack,
                resolvedExposedUnitIds =
                  Set.unions
                    (Map.elems sessionContext.ghcEnvironmentSnapshot.ghcEnvironmentSelectedUnitIdsByPackageName)
              }
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
        ( setPackageEnvironmentM initialPackageEnvironment
            . setGhciLikeDynFlags (parallelWorkersLimit sessionConfig)
            . setGhcWorkDirs (ghcWorkDir sessionConfig)
        )
      session <- GHC.getSession
      GHC.setSession (installDefinitionCallbacks sessionContext session)
