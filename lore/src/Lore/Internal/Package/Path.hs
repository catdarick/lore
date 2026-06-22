module Lore.Internal.Package.Path
  ( commonSetIntersection,
    componentMainModulePathCandidates,
    extractSourceDirs,
    firstExistingPath,
    isAncestorPath,
    normalizeRelativePath,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (nub)
import qualified Data.Set as Set
import Lore.Internal.Package.Types (ComponentData (..), PackageData (..))
import Lore.Internal.ProjectPath (isAncestorPath, normalProjectPath)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

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
normalizeRelativePath =
  normalProjectPath

firstExistingPath :: (MonadIO m) => [FilePath] -> m (Maybe FilePath)
firstExistingPath [] = pure Nothing
firstExistingPath (path : rest) = do
  exists <- liftIO (doesFileExist path)
  if exists
    then pure (Just path)
    else firstExistingPath rest

commonSetIntersection :: (Ord a) => [Set.Set a] -> Set.Set a
commonSetIntersection [] = Set.empty
commonSetIntersection sets = foldr1 Set.intersection sets

extractSourceDirs :: PackageData -> Set.Set FilePath
extractSourceDirs packageData = do
  Set.map (packageData.packageRoot </>) rawSourceDirs
  where
    rawSourceDirs = Set.unions $ map sourceDirs packageData.components
