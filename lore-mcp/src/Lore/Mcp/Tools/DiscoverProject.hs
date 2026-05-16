module Lore.Mcp.Tools.DiscoverProject
  ( discoverProjectTool,
  )
where

import Data.List (intercalate, isPrefixOf, isSuffixOf, sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lore
  ( ComponentData (..),
    Extension (..),
    GhcOption (..),
    MonadLore,
    PackageData (..),
    discoverProject,
    projectRootPath,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))
import System.FilePath (dropTrailingPathSeparator, makeRelative, normalise, splitDirectories, (</>))

discoverProjectTool :: (MonadLore m) => SomeTool m
discoverProjectTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "discoverProject",
        description = Just "Scans the workspace for Haskell package.yaml files to determine project structure. Useful for identifying available packages and their respective components (libraries, targets, executables).",
        handler = discoverProjectHandler
      }

discoverProjectHandler :: (MonadLore m) => m Text
discoverProjectHandler = do
  rootPath <- projectRootPath
  packages <- discoverProject
  pure (renderDiscoverProject rootPath (sortOn packageYamlPath packages))

renderDiscoverProject :: FilePath -> [PackageData] -> Text
renderDiscoverProject _ [] =
  "No package.yaml files were found under the project root."
renderDiscoverProject projectRoot packages =
  T.pack (intercalate "\n\n" (map (renderPackage projectRoot) packages))

renderPackage :: FilePath -> PackageData -> String
renderPackage projectRoot packageData =
  intercalate "\n\n" (packageBlock : componentBlocks)
  where
    sharedDependencies = commonSetIntersection (map dependencies packageData.components)
    sharedGhcOptions = commonSetIntersection (map (Set.map unGhcOption . ghcOptions) packageData.components)
    sharedExtensions = commonSetIntersection (map (Set.map unGhcExtension . defaultExtensions) packageData.components)

    packageBlock =
      intercalate
        "\n"
        [ "## Package: " <> packageData.packageName,
          "- package root: " <> renderDirectoryPath (toProjectRelativePath projectRoot packageData.packageRoot),
          "- package.yaml: " <> toProjectRelativePath projectRoot packageData.packageYamlPath,
          "- shared dependencies: " <> renderStringSet sharedDependencies,
          "- shared GHC options: " <> renderStringSet sharedGhcOptions,
          "- shared extensions: " <> renderStringSet sharedExtensions
        ]

    componentBlocks =
      case sortOn componentName packageData.components of
        [] -> ["### Component: (none)"]
        components ->
          map
            (renderComponent projectRoot packageData.packageRoot sharedDependencies sharedGhcOptions sharedExtensions)
            components

renderComponent :: FilePath -> FilePath -> Set.Set String -> Set.Set String -> Set.Set String -> ComponentData -> String
renderComponent projectRoot packageRoot sharedDependencies sharedGhcOptions sharedExtensions componentData =
  intercalate
    "\n"
    [ "### Component: " <> componentData.componentName,
      "- source dirs: " <> renderDirectorySet (Set.map (toProjectRelativePath projectRoot . (packageRoot </>)) componentData.sourceDirs),
      "- main module: " <> maybe "(none)" (toProjectRelativePath projectRoot) (resolveMainModulePath packageRoot componentData.sourceDirs componentData.mainModulePath),
      "- component specific dependencies: " <> renderStringSet (componentData.dependencies Set.\\ sharedDependencies),
      "- component specific GHC options: " <> renderStringSet (Set.map unGhcOption componentData.ghcOptions Set.\\ sharedGhcOptions),
      "- component specific extensions: " <> renderStringSet (Set.map unGhcExtension componentData.defaultExtensions Set.\\ sharedExtensions)
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
renderList values = intercalate ", " values
