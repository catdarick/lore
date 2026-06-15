module Lore.Tools.DirectoryTree.Gitignore
  ( GitignoredDirectoryMatcher,
    isHiddenDirectoryPath,
    loadGitignoredDirectoryMatchers,
    matchesGitignoredDirectory,
  )
where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Lore.Tools.Directory (isAncestorOrSelf, normalizeRelativePath)
import System.Directory (doesFileExist)
import System.FilePath (splitDirectories, (</>))

isHiddenDirectoryPath :: FilePath -> Bool
isHiddenDirectoryPath path =
  any isHiddenSegment (splitDirectories (normalizeRelativePath path))
  where
    isHiddenSegment = \case
      '.' : rest -> not (null rest) && rest /= "."
      _ -> False

data GitignoredDirectoryMatcher
  = GitignoredDirectoryName FilePath
  | GitignoredDirectoryPrefix FilePath
  deriving stock (Eq, Show)

loadGitignoredDirectoryMatchers :: FilePath -> IO [GitignoredDirectoryMatcher]
loadGitignoredDirectoryMatchers projectRootPath = do
  let gitignorePath = projectRootPath </> ".gitignore"
  exists <- doesFileExist gitignorePath
  if not exists
    then pure []
    else parseGitignoredDirectoryMatchers <$> readFile gitignorePath

parseGitignoredDirectoryMatchers :: String -> [GitignoredDirectoryMatcher]
parseGitignoredDirectoryMatchers content =
  mapMaybe parseGitignoredDirectoryMatcher (lines content)

parseGitignoredDirectoryMatcher :: String -> Maybe GitignoredDirectoryMatcher
parseGitignoredDirectoryMatcher rawLine = do
  let trimmedLine = trimLine rawLine
  normalizedPattern <- normalizeGitignoredDirectoryPattern trimmedLine
  pure $
    if '/' `elem` normalizedPattern
      then GitignoredDirectoryPrefix (normalizeRelativePath normalizedPattern)
      else GitignoredDirectoryName normalizedPattern

normalizeGitignoredDirectoryPattern :: String -> Maybe FilePath
normalizeGitignoredDirectoryPattern patternLine
  | null patternLine = Nothing
  | "#" `isPrefixOf` patternLine = Nothing
  | "!" `isPrefixOf` patternLine = Nothing
  | any (`elem` patternLine) ['*', '?', '['] = Nothing
  | otherwise =
      case dropTrailingSlash (dropLeadingSlash patternLine) of
        "" -> Nothing
        normalized -> Just normalized
  where
    dropLeadingSlash path =
      case path of
        '/' : rest -> rest
        _ -> path

    dropTrailingSlash path =
      reverse (dropWhile (== '/') (reverse path))

trimLine :: String -> String
trimLine =
  dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd predicate =
      reverse . dropWhile predicate . reverse

matchesGitignoredDirectory :: [GitignoredDirectoryMatcher] -> FilePath -> Bool
matchesGitignoredDirectory matchers path =
  any (`matches` normalizedPath) matchers
  where
    normalizedPath = normalizeRelativePath path
    segments = splitDirectories normalizedPath

    matches matcher candidatePath =
      case matcher of
        GitignoredDirectoryName directoryName ->
          directoryName `elem` segments
        GitignoredDirectoryPrefix prefix ->
          isAncestorOrSelf prefix candidatePath
