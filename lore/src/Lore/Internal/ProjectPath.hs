module Lore.Internal.ProjectPath
  ( absoluteProjectPath,
    displayProjectPath,
    isAncestorPath,
    normalProjectPath,
  )
where

import Data.List (isPrefixOf)
import System.FilePath (dropTrailingPathSeparator, isRelative, makeRelative, normalise, splitDirectories, (</>))

normalProjectPath :: FilePath -> FilePath
normalProjectPath path =
  case dropTrailingPathSeparator (normalise path) of
    "" -> "."
    normalized -> normalized

absoluteProjectPath :: FilePath -> FilePath -> FilePath
absoluteProjectPath projectRoot path
  | isRelative path = normalProjectPath (projectRoot </> path)
  | otherwise = normalProjectPath path

displayProjectPath :: FilePath -> FilePath -> FilePath
displayProjectPath projectRoot path
  | isRelative path = normalProjectPath path
  | otherwise = normalProjectPath (makeRelative projectRoot path)

isAncestorPath :: FilePath -> FilePath -> Bool
isAncestorPath ancestor path =
  splitDirectories (normalProjectPath ancestor)
    `isPrefixOf` splitDirectories (normalProjectPath path)
