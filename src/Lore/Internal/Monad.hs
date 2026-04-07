{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Monad
  ( MonadLore,
    LoreMonadT (..),
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.RWS (MonadReader (..), MonadTrans (lift))
import Control.Monad.Reader (MonadIO, ReaderT (..), asks)
import qualified GHC
import qualified GHC.Driver.Monad as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Utils.Exception as GHC
import qualified GHC.Utils.Logger as GHC
import Lore.Internal.Session (SessionContext (..))
import Lore.Logger (MonadLogger (..))
import UnliftIO (MonadUnliftIO (..))

type MonadLore m = (MonadReader SessionContext m, MonadIO m, GHC.GhcMonad m, MonadLogger m, MonadUnliftIO m)

newtype LoreMonadT m a = LoreMonadT {unLoreMonadT :: ReaderT SessionContext (GHC.GhcT m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance (MonadUnliftIO m) => MonadUnliftIO (LoreMonadT m) where
  withRunInIO inner =
    LoreMonadT $
      ReaderT $ \ctx ->
        GHC.GhcT $ \session ->
          withRunInIO $ \runInBase ->
            inner $ \(LoreMonadT action) ->
              runInBase (GHC.unGhcT (runReaderT action ctx) session)

instance (MonadIO m) => GHC.HasDynFlags (LoreMonadT m) where
  getDynFlags = LoreMonadT $ ReaderT $ const GHC.getDynFlags

instance (MonadIO m) => GHC.HasLogger (LoreMonadT m) where
  getLogger = LoreMonadT $ ReaderT $ const GHC.getLogger

instance (GHC.ExceptionMonad m) => GHC.GhcMonad (LoreMonadT m) where
  getSession = LoreMonadT $ ReaderT $ const GHC.getSession
  setSession s = LoreMonadT $ ReaderT $ const (GHC.setSession s)

instance MonadTrans LoreMonadT where
  lift = LoreMonadT . lift . GHC.liftGhcT

instance (Monad m) => MonadReader SessionContext (LoreMonadT m) where
  ask = LoreMonadT ask
  local f (LoreMonadT m) = LoreMonadT $ local f m

instance (MonadIO m) => MonadLogger (LoreMonadT m) where
  getLoggerHandle = asks loggerHandle
