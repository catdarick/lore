{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Mcp.Monad
  ( MonadLoreMcp (..),
    DefinitionCacheReplacement (..),
    LoreMcpContext (..),
    LoreMcpMonad (..),
    getSentDefinitionHashes,
    newLoreMcpContext,
    replaceSentDefinitionHashes,
    runLoreMcp,
    clearSentDefinitionHashes,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
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

data DefinitionCacheReplacement = DefinitionCacheReplacement
  { previousCachedDefinitionCount :: Int,
    currentCachedDefinitionCount :: Int
  }
  deriving (Eq, Show)

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

getSentDefinitionHashes :: (MonadLoreMcp m) => m (Set.Set Text)
getSentDefinitionHashes = do
  cache <- sentDefinitionHashes <$> getLoreMcpContext
  liftIO (readMVar cache)

replaceSentDefinitionHashes :: (MonadLoreMcp m) => Set.Set Text -> m DefinitionCacheReplacement
replaceSentDefinitionHashes newHashes = do
  cache <- sentDefinitionHashes <$> getLoreMcpContext
  liftIO $
    modifyMVar cache \oldHashes ->
      pure
        ( newHashes,
          DefinitionCacheReplacement
            { previousCachedDefinitionCount = Set.size oldHashes,
              currentCachedDefinitionCount = Set.size newHashes
            }
        )

clearSentDefinitionHashes :: (MonadLoreMcp m) => m Int
clearSentDefinitionHashes = do
  replacement <- replaceSentDefinitionHashes Set.empty
  pure replacement.previousCachedDefinitionCount
