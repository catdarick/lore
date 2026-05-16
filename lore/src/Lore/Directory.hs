module Lore.Directory
  ( DirectoryListOptions (..),
    defaultDirectoryListOptions,
    DirectoryPage (..),
    DirectoryPageEntry (..),
    DirectoryEntryType (..),
    DirectoryError (..),
    describeDirectoryError,
    listDirectoryPage,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Lore.Internal.Directory
  ( DirectoryEntry (..),
    DirectoryEntryType (..),
    DirectoryError (..),
    describeDirectoryError,
    listVisibleDirectoryEntries,
    relativeProjectPath,
    resolveDirectoryInsideProject,
  )
import Lore.Internal.Monad (MonadLore)
import Lore.Internal.Session (SessionContext (..))
import System.Directory (canonicalizePath)

data DirectoryListOptions = DirectoryListOptions
  { directoryListBasePath :: FilePath,
    directoryListSkip :: Int,
    directoryListPageSize :: Int
  }
  deriving stock (Eq, Show)

defaultDirectoryListOptions :: DirectoryListOptions
defaultDirectoryListOptions =
  DirectoryListOptions
    { directoryListBasePath = ".",
      directoryListSkip = 0,
      directoryListPageSize = 70
    }

data DirectoryPage = DirectoryPage
  { directoryPageRootPath :: FilePath,
    directoryPageRelativePath :: FilePath,
    directoryPageTotalEntries :: Int,
    directoryPageSkip :: Int,
    directoryPagePageSize :: Int,
    directoryPageEntries :: [DirectoryPageEntry]
  }
  deriving stock (Eq, Show)

data DirectoryPageEntry = DirectoryPageEntry
  { directoryPageEntryName :: FilePath,
    directoryPageEntryType :: DirectoryEntryType
  }
  deriving stock (Eq, Show)

listDirectoryPage :: (MonadLore m) => DirectoryListOptions -> m (Either DirectoryError DirectoryPage)
listDirectoryPage options = do
  projectRootPath <- asks projectRoot
  canonicalProjectRoot <- liftIO $ canonicalizePath projectRootPath

  let skip = max 0 (directoryListSkip options)
      pageSize = max 1 (directoryListPageSize options)

  resolved <- liftIO $ resolveDirectoryInsideProject projectRootPath (directoryListBasePath options)

  case resolved of
    Left err -> pure (Left err)
    Right directoryPath -> do
      entries <- liftIO $ listVisibleDirectoryEntries directoryPath
      pure $
        Right
          DirectoryPage
            { directoryPageRootPath = directoryPath,
              directoryPageRelativePath = relativeProjectPath canonicalProjectRoot directoryPath,
              directoryPageTotalEntries = length entries,
              directoryPageSkip = skip,
              directoryPagePageSize = pageSize,
              directoryPageEntries = take pageSize (drop skip (map toPageEntry entries))
            }
  where
    toPageEntry entry =
      DirectoryPageEntry
        { directoryPageEntryName = directoryEntryName entry,
          directoryPageEntryType = directoryEntryType entry
        }
