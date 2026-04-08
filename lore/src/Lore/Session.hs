module Lore.Session
  ( SessionContext (..),
    SessionConfig (..),
    PreludeImportRule (..),
    defaultSessionConfig,
    prepareSessionContext,
    runLore,
    ParallelWorkersCount (..),
  )
where

import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified GHC
import qualified GHC.Paths as GHCPaths
import qualified GHC.Utils.Exception as GHCException
import Lore.Internal.Ghc.DynFlags
  ( ParallelWorkersCount (..),
    modifySessionDynFlags,
    setGhcWorkDirs,
    setGhciLikeDynFlags,
    setPackageDbs,
  )
import Lore.Internal.Monad (LoreMonadT (..))
import Lore.Internal.Session
  ( PreludeImportRule (..),
    SessionConfig (..),
    SessionContext (..),
    defaultSessionConfig,
    prepareSessionContext,
  )
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))

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
        (\_ -> GHC.runGhcT (Just GHCPaths.libdir) $ setupGhcSession sessionContext >> runReaderT (unLoreMonadT lore) sessionContext)
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
      modifySessionDynFlags $
        setGhcWorkDirs (ghcWorkDir sessionConfig)
          . setGhciLikeDynFlags (parallelWorkersLimit sessionConfig)
          . setPackageDbs (packageDbPaths sessionContext)
