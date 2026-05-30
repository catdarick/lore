module Lore.Internal.File
  ( findFilesByExtensionRecursively,
    shouldIgnoreDirectory,
  )
where

import Control.Monad (forM)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (splitDirectories, takeExtension, (</>))

newtype DirectoryIgnoreList = DirectoryIgnoreList (Set.Set String)

findFilesByExtensionRecursively :: Maybe DirectoryIgnoreList -> FilePath -> String -> IO [FilePath]
findFilesByExtensionRecursively ignoreList root extension = do
  entries <- listDirectory root
  concat <$> forM entries \entry -> do
    let path = root </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then
        if shouldIgnoreDirectory ignoreList entry
          then pure []
          else findFilesByExtensionRecursively ignoreList path extension
      else pure [path | takeExtension path == extension]

shouldIgnoreDirectory :: Maybe DirectoryIgnoreList -> FilePath -> Bool
shouldIgnoreDirectory ignoreList dir =
  case ignoreList of
    Just (DirectoryIgnoreList ignoreSet) -> any (`Set.member` ignoreSet) (splitDirectories dir)
    Nothing -> False
