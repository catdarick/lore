module Lore where

import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import qualified GHC
import GHC.DynFlags (modifySessionDynFlags, setGhcWorkDirs, setGhciLikeDynFlags, setPackageDbs)
import qualified GHC.Paths as GHC
import qualified GHC.Utils.Exception as GHC
import Monad
import Session
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))

runLoreMonadT :: (GHC.ExceptionMonad m) => SessionConfig -> LoreMonadT m a -> m a
runLoreMonadT sessionConfig lore = do
  eiSessionContext <- liftIO $ prepareSessionContext sessionConfig
  case eiSessionContext of
    Left err -> do
      error err
    Right sessionContext@SessionContext {projectRoot = sessionProjectRoot} -> do
      bracket
        ( liftIO do
            cwd <- getCurrentDirectory
            setCurrentDirectory sessionProjectRoot
            pure cwd
        )
        (liftIO . setCurrentDirectory)
        (\_ -> GHC.runGhcT (Just GHC.libdir) $ setupGhcSession sessionContext >> runReaderT (runLore lore) sessionContext)
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
