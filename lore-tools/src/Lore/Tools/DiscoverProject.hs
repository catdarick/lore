module Lore.Tools.DiscoverProject
  ( DiscoverProjectOutput (..),
    discoverProject,
    renderDiscoverProject,
  )
where

import Control.Monad.RWS (asks)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (catMaybes, listToMaybe)
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
import Lore.Tools.Render.Doc (LoreDoc, bulletList, heading1, heading2, heading3, paragraph)
import Lore.Tools.Render.Text (renderList)
import System.FilePath (makeRelative, normalise, (</>))

data DiscoverProjectOutput = DiscoverProjectOutput
  { discoverProjectRootPath :: FilePath,
    discoverProjectPackages :: [PackageData]
  }

data BuildSettings = BuildSettings
  { buildDependencies :: Set.Set String,
    buildGhcOptions :: Set.Set String,
    buildExtensions :: Set.Set String
  }

discoverProject :: (MonadLore m) => m DiscoverProjectOutput
discoverProject = do
  rootPath <- asks projectRoot
  packages <- Project.discoverProject
  pure
    DiscoverProjectOutput
      { discoverProjectRootPath = rootPath,
        discoverProjectPackages = sortOn packageManifestPath packages
      }

renderDiscoverProject :: DiscoverProjectOutput -> LoreDoc
renderDiscoverProject output =
  renderDiscoverProjectFromPackages output.discoverProjectRootPath output.discoverProjectPackages

renderDiscoverProjectFromPackages :: FilePath -> [PackageData] -> LoreDoc
renderDiscoverProjectFromPackages _ [] =
  paragraph "No package manifests were found under the project root."
renderDiscoverProjectFromPackages projectRoot packages =
  renderWorkspace workspaceSharedSettings
    <> mconcat (map (renderPackage projectRoot workspaceSharedSettings) packages)
  where
    workspaceSharedSettings = sharedBuildSettings (concatMap components packages)

renderWorkspace :: BuildSettings -> LoreDoc
renderWorkspace sharedSettings =
  heading1 "Workspace"
    <> bulletList (renderBuildSettings "shared" sharedSettings)

renderPackage :: FilePath -> BuildSettings -> PackageData -> LoreDoc
renderPackage projectRoot workspaceSharedSettings packageData =
  heading2 ("Package: " <> T.pack packageData.packageName)
    <> bulletList
      ( [ paragraph ("package root: " <> T.pack (renderDirectoryPath (toProjectRelativePath projectRoot packageData.packageRoot))),
          paragraph ("package manifest: " <> T.pack (toProjectRelativePath projectRoot packageData.packageManifestPath))
        ]
          <> renderBuildSettings "package shared" packageOnlySettings
      )
    <> mconcat componentDocs
  where
    packageSharedSettings = sharedBuildSettings packageData.components
    packageOnlySettings = differenceBuildSettings packageSharedSettings workspaceSharedSettings

    componentDocs =
      case sortOn componentName packageData.components of
        [] -> [heading3 "Component: (none)"]
        components ->
          map
            (renderComponent projectRoot packageData.packageRoot packageSharedSettings)
            components

renderComponent :: FilePath -> FilePath -> BuildSettings -> ComponentData -> LoreDoc
renderComponent projectRoot packageRoot packageSharedSettings componentData =
  heading3 ("Component: " <> T.pack componentData.componentName)
    <> bulletList
      ( [ paragraph ("source dirs: " <> T.pack (renderDirectorySet (Set.map (toProjectRelativePath projectRoot . (packageRoot </>)) componentData.sourceDirs))),
          paragraph ("main module: " <> maybe "(none)" (T.pack . toProjectRelativePath projectRoot) (listToMaybe (componentMainModulePathCandidates packageRoot componentData)))
        ]
          <> renderBuildSettings "component specific" componentOnlySettings
      )
  where
    componentOnlySettings = differenceBuildSettings (componentBuildSettings componentData) packageSharedSettings

sharedBuildSettings :: [ComponentData] -> BuildSettings
sharedBuildSettings componentsData =
  BuildSettings
    { buildDependencies = commonSetIntersection (map dependencies componentsData),
      buildGhcOptions = commonSetIntersection (map (Set.map unGhcOption . ghcOptions) componentsData),
      buildExtensions = commonSetIntersection (map (Set.map unGhcExtension . defaultExtensions) componentsData)
    }

componentBuildSettings :: ComponentData -> BuildSettings
componentBuildSettings componentData =
  BuildSettings
    { buildDependencies = componentData.dependencies,
      buildGhcOptions = Set.map unGhcOption componentData.ghcOptions,
      buildExtensions = Set.map unGhcExtension componentData.defaultExtensions
    }

differenceBuildSettings :: BuildSettings -> BuildSettings -> BuildSettings
differenceBuildSettings settings inheritedSettings =
  BuildSettings
    { buildDependencies = settings.buildDependencies Set.\\ inheritedSettings.buildDependencies,
      buildGhcOptions = settings.buildGhcOptions Set.\\ inheritedSettings.buildGhcOptions,
      buildExtensions = settings.buildExtensions Set.\\ inheritedSettings.buildExtensions
    }

renderBuildSettings :: T.Text -> BuildSettings -> [LoreDoc]
renderBuildSettings scope settings =
  catMaybes
    [ renderSetting (scope <> " dependencies") settings.buildDependencies,
      renderSetting (scope <> " GHC options") settings.buildGhcOptions,
      renderSetting (scope <> " extensions") settings.buildExtensions
    ]

renderSetting :: T.Text -> Set.Set String -> Maybe LoreDoc
renderSetting label values
  | Set.null values = Nothing
  | otherwise = Just (paragraph (label <> ": " <> renderStringSet values))

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
