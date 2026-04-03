module Internal.File where

import Control.Monad (forM)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (splitDirectories, takeFileName, (</>))

newtype DirectoryIgnoreList = DirectoryIgnoreList
  { unIgnoreList :: Set.Set String
  }

defaultIgnoreList :: DirectoryIgnoreList
defaultIgnoreList = DirectoryIgnoreList $ Set.fromList [".git", ".stack-work", "dist-newstyle", "dist", ".ghci-work"]

findFilesByNameRecursively :: Maybe DirectoryIgnoreList -> FilePath -> String -> IO [FilePath]
findFilesByNameRecursively ignoreList root targetName = do
  entries <- listDirectory root
  concat <$> forM entries \entry -> do
    let path = root </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then
        if shouldIgnore entry
          then pure []
          else findFilesByNameRecursively ignoreList path targetName
      else pure [path | takeFileName path == targetName]
  where
    shouldIgnore dir = case ignoreList of
      Just (DirectoryIgnoreList ignoreSet) -> any (`Set.member` ignoreSet) (splitDirectories dir)
      Nothing -> False
