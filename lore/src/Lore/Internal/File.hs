module Lore.Internal.File
  ( defaultIgnoreList,
    findFilesByNameRecursively,
    shouldIgnoreDirectory,
  )
where

import Control.Monad (forM)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (splitDirectories, takeFileName, (</>))

newtype DirectoryIgnoreList = DirectoryIgnoreList (Set.Set String)

defaultIgnoreList :: DirectoryIgnoreList
defaultIgnoreList = DirectoryIgnoreList $ Set.fromList [".git", ".stack-work", "dist-newstyle", "dist", ".ghci-work", "bench-fixtures"]

findFilesByNameRecursively :: Maybe DirectoryIgnoreList -> FilePath -> String -> IO [FilePath]
findFilesByNameRecursively ignoreList root targetName = do
  entries <- listDirectory root
  concat <$> forM entries \entry -> do
    let path = root </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then
        if shouldIgnoreDirectory ignoreList entry
          then pure []
          else findFilesByNameRecursively ignoreList path targetName
      else pure [path | takeFileName path == targetName]

shouldIgnoreDirectory :: Maybe DirectoryIgnoreList -> FilePath -> Bool
shouldIgnoreDirectory ignoreList dir =
  case ignoreList of
    Just (DirectoryIgnoreList ignoreSet) -> any (`Set.member` ignoreSet) (splitDirectories dir)
    Nothing -> False
