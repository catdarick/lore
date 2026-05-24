module Lore.Mcp.Tools.DiscoverProject
  ( discoverProjectTool,
  )
where

import Control.Monad.RWS (asks)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Lore.Mcp.Internal.LoreDoc (LoreDoc, bulletList, heading2, heading3, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))
import Lore.Mcp.Tools.Shared.Rendering (renderList)
import Lore.Monad (MonadLore)
import Lore.Project
  ( ComponentData (..),
    Extension (..),
    GhcOption (..),
    PackageData (..),
    commonSetIntersection,
    componentMainModulePathCandidates,
    discoverProject,
    normalizeRelativePath,
  )
import Lore.Session (SessionContext (..))
import System.FilePath (makeRelative, normalise, (</>))

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
        paragraph ("shared dependencies: " <> renderStringSet sharedDependencies),
        paragraph ("shared GHC options: " <> renderStringSet sharedGhcOptions),
        paragraph ("shared extensions: " <> renderStringSet sharedExtensions)
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
        paragraph ("main module: " <> maybe "(none)" (T.pack . toProjectRelativePath projectRoot) (listToMaybe (componentMainModulePathCandidates packageRoot componentData))),
        paragraph ("component specific dependencies: " <> renderStringSet (componentData.dependencies Set.\\ sharedDependencies)),
        paragraph ("component specific GHC options: " <> renderStringSet (Set.map unGhcOption componentData.ghcOptions Set.\\ sharedGhcOptions)),
        paragraph ("component specific extensions: " <> renderStringSet (Set.map unGhcExtension componentData.defaultExtensions Set.\\ sharedExtensions))
      ]

renderStringSet :: Set.Set String -> T.Text
renderStringSet values =
  renderList (map T.pack (Set.toAscList values))

renderDirectorySet :: Set.Set FilePath -> String
renderDirectorySet values =
  T.unpack (renderList (map (T.pack . renderDirectoryPath) (Set.toAscList values)))

renderDirectoryPath :: FilePath -> FilePath
renderDirectoryPath path
  | path == "." = "./"
  | "/" `isSuffixOf` path = path
  | otherwise = path <> "/"

toProjectRelativePath :: FilePath -> FilePath -> FilePath
toProjectRelativePath projectRoot path =
  normalizeRelativePath (makeRelative (normalise projectRoot) (normalise path))
