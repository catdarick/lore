{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Mcp.Monad
  ( MonadLoreMcp (..),
    LoreMcpContext (..),
    LoreMcpMonad (..),
    newLoreMcpContext,
    runLoreMcp,
    clearSentDefinitionHashes,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), MonadTrans (lift), ReaderT (runReaderT))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Utils.Logger as GHC
import Lore (LoreMonadT, MonadLore)
import Lore.Logger (MonadLogger)
import Lore.Session (SessionConfig, SessionContext, runLore)
import UnliftIO (MonadUnliftIO)

data LoreMcpContext = LoreMcpContext
  { sentDefinitionHashes :: MVar (Set.Set Text),
    enableDefinitionKnowledgeCache :: Bool
  }

newLoreMcpContext :: Bool -> IO LoreMcpContext
newLoreMcpContext enableDefinitionKnowledgeCache = do
  sentDefinitionHashes <- newMVar Set.empty
  pure LoreMcpContext {sentDefinitionHashes, enableDefinitionKnowledgeCache}

newtype LoreMcpMonad a = LoreMcpMonad {unLoreMcpMonad :: LoreMonadT (ReaderT LoreMcpContext IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO)
  deriving newtype (GHC.HasLogger, GHC.HasDynFlags, GHC.GhcMonad)
  deriving newtype (MonadReader SessionContext, MonadLogger)

class (MonadLore m) => MonadLoreMcp m where
  getLoreMcpContext :: m LoreMcpContext

instance MonadLoreMcp LoreMcpMonad where
  getLoreMcpContext = LoreMcpMonad $ lift ask

runLoreMcp :: SessionConfig -> LoreMcpContext -> LoreMcpMonad a -> IO a
runLoreMcp loreConfig context (LoreMcpMonad action) = do
  runReaderT (runLore loreConfig action) context

clearSentDefinitionHashes :: (MonadLoreMcp m) => m Int
clearSentDefinitionHashes = do
  cache <- sentDefinitionHashes <$> getLoreMcpContext
  liftIO $
    modifyMVar cache \knownHashes ->
      pure (Set.empty, Set.size knownHashes)
