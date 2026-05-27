module Lore.Tools.DiscoverProject
  ( DiscoverProjectOutput (..),
    discoverProject,
    renderDiscoverProject,
  )
where

import Control.Monad.RWS (asks)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Lore.Monad (MonadLore)
import qualified Lore.Project as Project
import Lore.Project
  ( ComponentData (..),
    Extension (..),
    GhcOption (..),
    PackageData (..),
    commonSetIntersection,
    componentMainModulePathCandidates,
    normalizeRelativePath,
  )
import Lore.Session (SessionContext (..))
import Lore.Tools.Render.Doc (LoreDoc, bulletList, heading2, heading3, paragraph)
import Lore.Tools.Render.Text (renderList)
import System.FilePath (makeRelative, normalise, (</>))

data DiscoverProjectOutput = DiscoverProjectOutput
  { discoverProjectRootPath :: FilePath,
    discoverProjectPackages :: [PackageData]
  }

discoverProject :: (MonadLore m) => m DiscoverProjectOutput
discoverProject = do
  rootPath <- asks projectRoot
  packages <- Project.discoverProject
  pure
    DiscoverProjectOutput
      { discoverProjectRootPath = rootPath,
        discoverProjectPackages = sortOn packageYamlPath packages
      }

renderDiscoverProject :: DiscoverProjectOutput -> LoreDoc
renderDiscoverProject output =
  renderDiscoverProjectFromPackages output.discoverProjectRootPath output.discoverProjectPackages

renderDiscoverProjectFromPackages :: FilePath -> [PackageData] -> LoreDoc
renderDiscoverProjectFromPackages _ [] =
  paragraph "No package.yaml files were found under the project root."
renderDiscoverProjectFromPackages projectRoot packages =
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
