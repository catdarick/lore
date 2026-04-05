{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.RWS (MonadReader (..), MonadTrans (lift))
import Control.Monad.Reader (MonadIO, ReaderT (..), asks)
import qualified GHC
import qualified GHC.Driver.Monad as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Utils.Exception as GHC
import qualified GHC.Utils.Logger as GHC
import Internal.Logger (MonadLogger (..))
import Session (SessionContext (..))
import UnliftIO (MonadUnliftIO (..))

type MonadLore m = (MonadReader SessionContext m, MonadIO m, GHC.GhcMonad m, MonadLogger m, MonadUnliftIO m)

newtype LoreMonadT m a = LoreMonad {runLore :: ReaderT SessionContext (GHC.GhcT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance (MonadUnliftIO m) => MonadUnliftIO (LoreMonadT m) where
  withRunInIO inner =
    LoreMonad $
      ReaderT $ \ctx ->
        GHC.GhcT $ \session ->
          withRunInIO $ \runInBase ->
            inner $ \(LoreMonad action) ->
              runInBase (GHC.unGhcT (runReaderT action ctx) session)

instance (MonadIO m) => GHC.HasDynFlags (LoreMonadT m) where
  getDynFlags = LoreMonad $ ReaderT $ const GHC.getDynFlags

instance (MonadIO m) => GHC.HasLogger (LoreMonadT m) where
  getLogger = LoreMonad $ ReaderT $ const GHC.getLogger

instance (GHC.ExceptionMonad m) => GHC.GhcMonad (LoreMonadT m) where
  getSession = LoreMonad $ ReaderT $ const GHC.getSession
  setSession s = LoreMonad $ ReaderT $ const (GHC.setSession s)

instance MonadTrans LoreMonadT where
  lift = LoreMonad . lift . GHC.liftGhcT

instance (Monad m) => MonadReader SessionContext (LoreMonadT m) where
  ask = LoreMonad ask
  local f (LoreMonad m) = LoreMonad $ local f m

instance (MonadIO m) => MonadLogger (LoreMonadT m) where
  getLoggerHandle = asks loggerHandle
