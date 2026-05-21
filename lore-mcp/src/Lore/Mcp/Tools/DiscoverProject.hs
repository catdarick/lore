module Lore.Mcp.Tools.DiscoverProject
  ( discoverProjectTool,
  )
where

import Control.Monad.RWS (asks)
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import qualified Data.Set as Set
import qualified Data.Text as T
import Lore
  ( ComponentData (..),
    Extension (..),
    GhcOption (..),
    MonadLore,
    PackageData (..),
    discoverProject,
  )
import Lore.Mcp.Internal.LoreDoc (LoreDoc, bulletList, heading2, heading3, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))
import Lore.Session (SessionContext (..))
import System.FilePath (dropTrailingPathSeparator, makeRelative, normalise, splitDirectories, (</>))

discoverProjectTool :: (MonadLore m) => SomeTool m
discoverProjectTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "discoverProject",
        description = Just "Scans the workspace for Haskell package.yaml files to determine project structure. Useful for identifying available packages and their respective components (libraries, targets, executables).",
        handler = discoverProjectHandler
      }

discoverProjectHandler :: (MonadLore m) => m LoreDoc
discoverProjectHandler = do
  rootPath <- asks projectRoot
  packages <- discoverProject
  pure (renderDiscoverProject rootPath (sortOn packageYamlPath packages))

renderDiscoverProject :: FilePath -> [PackageData] -> LoreDoc
renderDiscoverProject _ [] =
  paragraph "No package.yaml files were found under the project root."
renderDiscoverProject projectRoot packages =
  mconcat (map (renderPackage projectRoot) packages)

renderPackage :: FilePath -> PackageData -> LoreDoc
renderPackage projectRoot packageData =
  heading2 ("Package: " <> T.pack packageData.packageName)
    <> bulletList
      [ paragraph ("package root: " <> T.pack (renderDirectoryPath (toProjectRelativePath projectRoot packageData.packageRoot))),
        paragraph ("package.yaml: " <> T.pack (toProjectRelativePath projectRoot packageData.packageYamlPath)),
        paragraph ("shared dependencies: " <> T.pack (renderStringSet sharedDependencies)),
        paragraph ("shared GHC options: " <> T.pack (renderStringSet sharedGhcOptions)),
        paragraph ("shared extensions: " <> T.pack (renderStringSet sharedExtensions))
      ]
    <> mconcat componentDocs
  where
    sharedDependencies = commonSetIntersection (map dependencies packageData.components)
    sharedGhcOptions = commonSetIntersection (map (Set.map unGhcOption . ghcOptions) packageData.components)
    sharedExtensions = commonSetIntersection (map (Set.map unGhcExtension . defaultExtensions) packageData.components)

    componentDocs =
      case sortOn componentName packageData.components of
        [] -> [heading3 "Component: (none)"]
        components ->
          map
            (renderComponent projectRoot packageData.packageRoot sharedDependencies sharedGhcOptions sharedExtensions)
            components

renderComponent :: FilePath -> FilePath -> Set.Set String -> Set.Set String -> Set.Set String -> ComponentData -> LoreDoc
renderComponent projectRoot packageRoot sharedDependencies sharedGhcOptions sharedExtensions componentData =
  heading3 ("Component: " <> T.pack componentData.componentName)
    <> bulletList
      [ paragraph ("source dirs: " <> T.pack (renderDirectorySet (Set.map (toProjectRelativePath projectRoot . (packageRoot </>)) componentData.sourceDirs))),
        paragraph ("main module: " <> maybe "(none)" (T.pack . toProjectRelativePath projectRoot) (resolveMainModulePath packageRoot componentData.sourceDirs componentData.mainModulePath)),
        paragraph ("component specific dependencies: " <> T.pack (renderStringSet (componentData.dependencies Set.\\ sharedDependencies))),
        paragraph ("component specific GHC options: " <> T.pack (renderStringSet (Set.map unGhcOption componentData.ghcOptions Set.\\ sharedGhcOptions))),
        paragraph ("component specific extensions: " <> T.pack (renderStringSet (Set.map unGhcExtension componentData.defaultExtensions Set.\\ sharedExtensions)))
      ]

renderStringSet :: Set.Set String -> String
renderStringSet values =
  renderList (Set.toAscList values)

renderDirectorySet :: Set.Set FilePath -> String
renderDirectorySet values =
  renderList (map renderDirectoryPath (Set.toAscList values))

renderDirectoryPath :: FilePath -> FilePath
renderDirectoryPath path
  | path == "." = "./"
  | "/" `isSuffixOf` path = path
  | otherwise = path <> "/"

toProjectRelativePath :: FilePath -> FilePath -> FilePath
toProjectRelativePath projectRoot path =
  normalizeRelativePath (makeRelative (normalise projectRoot) (normalise path))

normalizeRelativePath :: FilePath -> FilePath
normalizeRelativePath path =
  case dropTrailingPathSeparator (normalise path) of
    "" -> "."
    normalized -> normalized

resolveMainModulePath :: FilePath -> Set.Set FilePath -> Maybe FilePath -> Maybe FilePath
resolveMainModulePath _ _ Nothing = Nothing
resolveMainModulePath packageRoot sourceDirSet (Just mainPath) =
  Just (packageRoot </> normalizedMainPathFromRoot)
  where
    sourceDirs = Set.toAscList sourceDirSet
    normalizedMainPath = normalizeRelativePath mainPath
    normalizedMainPathFromRoot =
      if any (`isAncestorPath` normalizedMainPath) sourceDirs
        then normalizedMainPath
        else case sourceDirs of
          [singleSourceDir] -> normalizeRelativePath (singleSourceDir </> normalizedMainPath)
          _ -> normalizedMainPath

isAncestorPath :: FilePath -> FilePath -> Bool
isAncestorPath ancestor path =
  splitDirectories (normalizeRelativePath ancestor)
    `isPrefixOf` splitDirectories (normalizeRelativePath path)

commonSetIntersection :: (Ord a) => [Set.Set a] -> Set.Set a
commonSetIntersection [] = Set.empty
commonSetIntersection sets = foldr1 Set.intersection sets

renderList :: [String] -> String
renderList [] = "(none)"
renderList values = T.unpack (T.intercalate ", " (map T.pack values))
