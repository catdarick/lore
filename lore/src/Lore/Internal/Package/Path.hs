module Lore.Internal.Package.Path
  ( commonSetIntersection,
    componentMainModulePathCandidates,
    extractDependencies,
    extractSourceDirs,
    firstExistingPath,
    isAncestorPath,
    normalizeRelativePath,
  )
where

import Data.List (isPrefixOf, nub)
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)
import Lore.Internal.Package.Types (ComponentData (..), PackageData (..))
import Lore.Monad (MonadLore)
import System.Directory (doesFileExist)
import System.FilePath (dropTrailingPathSeparator, normalise, splitDirectories, (</>))

componentMainModulePathCandidates :: FilePath -> ComponentData -> [FilePath]
componentMainModulePathCandidates packageRoot component =
  case component.mainModulePath of
    Nothing -> []
    Just mainPath ->
      nub (preferredMainPath : fallbackCandidates)
      where
        preferredMainPath = packageRoot </> normalizedMainPathFromRoot
        sourceDirs = Set.toAscList component.sourceDirs
        normalizedMainPath = normalizeRelativePath mainPath
        normalizedMainPathFromRoot =
          if any (`isAncestorPath` normalizedMainPath) sourceDirs
            then normalizedMainPath
            else case sourceDirs of
              [singleSourceDir] -> normalizeRelativePath (singleSourceDir </> normalizedMainPath)
              _ -> normalizedMainPath
        fallbackCandidates =
          map resolveThroughSourceDir sourceDirs
            <> [packageRoot </> normalizedMainPath]

        resolveThroughSourceDir sourceDir
          | sourceDir `isAncestorPath` normalizedMainPath =
              packageRoot </> normalizedMainPath
          | otherwise =
              packageRoot </> normalizeRelativePath (sourceDir </> normalizedMainPath)

normalizeRelativePath :: FilePath -> FilePath
normalizeRelativePath path =
  case dropTrailingPathSeparator (normalise path) of
    "" -> "."
    normalized -> normalized

isAncestorPath :: FilePath -> FilePath -> Bool
isAncestorPath ancestor path =
  splitDirectories (normalizeRelativePath ancestor)
    `isPrefixOf` splitDirectories (normalizeRelativePath path)

firstExistingPath :: (MonadLore m) => [FilePath] -> m (Maybe FilePath)
firstExistingPath [] = pure Nothing
firstExistingPath (path : rest) = do
  exists <- liftIO (doesFileExist path)
  if exists
    then pure (Just path)
    else firstExistingPath rest

commonSetIntersection :: (Ord a) => [Set.Set a] -> Set.Set a
commonSetIntersection [] = Set.empty
commonSetIntersection sets = foldr1 Set.intersection sets

extractDependencies :: [ComponentData] -> Set.Set String
extractDependencies components =
  Set.unions (map dependencies components)

extractSourceDirs :: PackageData -> Set.Set FilePath
extractSourceDirs packageData = do
  Set.map (packageData.packageRoot </>) rawSourceDirs
  where
    rawSourceDirs = Set.unions $ map sourceDirs packageData.components
