module Lore.Internal.Directory
  ( DirectoryEntry (..),
    DirectoryEntryType (..),
    DirectoryError (..),
    describeDirectoryError,
    withCurrentDirectoryIO,
    listVisibleDirectoryEntries,
    resolveDirectoryInsideProject,
    relativeProjectPath,
    appendRelativePath,
    normalizeRelativePath,
    displayRelativePath,
    displayDirectoryName,
    isAncestorOrSelf,
    isInsideDirectory,
    isInsideRelativePath,
  )
where

import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (catMaybes)
import Lore.Internal.File (defaultIgnoreList, shouldIgnoreDirectory)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    makeAbsolute,
    withCurrentDirectory,
  )
import System.FilePath
  ( dropTrailingPathSeparator,
    isAbsolute,
    isRelative,
    makeRelative,
    normalise,
    splitDirectories,
    takeFileName,
    (</>),
  )

data DirectoryEntryType
  = DirectoryEntryDirectory
  | DirectoryEntryFile
  deriving stock (Eq, Ord, Show)

data DirectoryEntry = DirectoryEntry
  { directoryEntryName :: FilePath,
    directoryEntryType :: DirectoryEntryType
  }
  deriving stock (Eq, Show)

data DirectoryError
  = DirectoryNotFound FilePath
  | DirectoryExpectedDirectory FilePath
  | DirectoryOutsideProject FilePath
  deriving stock (Eq, Show)

describeDirectoryError :: DirectoryError -> String
describeDirectoryError = \case
  DirectoryNotFound path -> "Directory does not exist: " <> path
  DirectoryExpectedDirectory path -> "Path is not a directory: " <> path
  DirectoryOutsideProject path -> "Path is outside the project root: " <> path

withCurrentDirectoryIO :: FilePath -> IO a -> IO a
withCurrentDirectoryIO = withCurrentDirectory

resolveDirectoryInsideProject :: FilePath -> FilePath -> IO (Either DirectoryError FilePath)
resolveDirectoryInsideProject projectRootPath requestedPath = do
  absoluteProjectRoot <- canonicalizePath projectRootPath
  candidatePath <-
    if isAbsolute requestedPath
      then pure requestedPath
      else makeAbsolute (projectRootPath </> requestedPath)

  directoryExists <- doesDirectoryExist candidatePath
  fileExists <- doesFileExist candidatePath

  if directoryExists
    then do
      canonicalCandidate <- canonicalizePath candidatePath
      pure $
        if isInsideDirectory absoluteProjectRoot canonicalCandidate
          then Right canonicalCandidate
          else Left (DirectoryOutsideProject requestedPath)
    else
      if fileExists
        then pure (Left (DirectoryExpectedDirectory requestedPath))
        else pure (Left (DirectoryNotFound requestedPath))

listVisibleDirectoryEntries :: FilePath -> IO [DirectoryEntry]
listVisibleDirectoryEntries directoryPath = do
  entryNames <- listDirectory directoryPath
  entries <-
    forM entryNames \entryName -> do
      let entryPath = directoryPath </> entryName
      isDirectory <- doesDirectoryExist entryPath
      isFile <- doesFileExist entryPath

      pure $
        if isDirectory
          then Just DirectoryEntry {directoryEntryName = entryName, directoryEntryType = DirectoryEntryDirectory}
          else
            if isFile
              then Just DirectoryEntry {directoryEntryName = entryName, directoryEntryType = DirectoryEntryFile}
              else Nothing

  pure $
    sortEntries $
      filter (not . ignoredDirectory) $
        catMaybes entries
  where
    ignoredDirectory entry =
      directoryEntryType entry == DirectoryEntryDirectory
        && shouldIgnoreDirectory (Just defaultIgnoreList) (directoryEntryName entry)

sortEntries :: [DirectoryEntry] -> [DirectoryEntry]
sortEntries =
  sortOn \entry ->
    ( directoryEntryType entry,
      map toLower (directoryEntryName entry)
    )

appendRelativePath :: FilePath -> FilePath -> FilePath
appendRelativePath base child
  | base == "." = normalizeRelativePath child
  | otherwise = normalizeRelativePath (base </> child)

displayRelativePath :: FilePath -> FilePath -> FilePath
displayRelativePath rootRelativePath relativePath
  | rootRelativePath == "." = normalizeRelativePath relativePath
  | relativePath == "." = rootRelativePath
  | otherwise = rootRelativePath </> relativePath

displayDirectoryName :: FilePath -> FilePath
displayDirectoryName relativePath
  | relativePath == "." = "."
  | otherwise = takeFileName relativePath

normalizeRelativePath :: FilePath -> FilePath
normalizeRelativePath path =
  case dropTrailingPathSeparator (normalise path) of
    "" -> "."
    normalized -> normalized

relativeProjectPath :: FilePath -> FilePath -> FilePath
relativeProjectPath projectRootPath path =
  normalizeRelativePath (makeRelative (normalise projectRootPath) (normalise path))

isAncestorOrSelf :: FilePath -> FilePath -> Bool
isAncestorOrSelf ancestor path
  | normalizedAncestor == "." = True
  | otherwise = splitDirectories normalizedAncestor `isPrefixOf` splitDirectories normalizedPath
  where
    normalizedAncestor = normalizeRelativePath ancestor
    normalizedPath = normalizeRelativePath path

isInsideDirectory :: FilePath -> FilePath -> Bool
isInsideDirectory parent child =
  relative == "." || isInsideRelativePath relative
  where
    relative = makeRelative parent child

isInsideRelativePath :: FilePath -> Bool
isInsideRelativePath path =
  path == "." || (isRelative path && not (".." `elem` splitDirectories path))
