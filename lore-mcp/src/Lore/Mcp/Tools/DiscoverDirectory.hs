module Lore.Mcp.Tools.DiscoverDirectory
  ( discoverDirectoryTool,
  )
where

import qualified Data.Aeson as J
import Data.List (intercalate)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.DirectoryTree
  ( DirectoryTree (..),
    DirectoryTreeChild (..),
    DirectoryTreeDiscoveryOptions (..),
    DirectoryTreeFile (..),
    DirectoryTreeNode (..),
    DirectoryTreeNoisyDirectoryOptions (..),
    DirectoryTreeOmittedEntries (..),
    DirectoryTreeStats (..),
    defaultDirectoryTreeDiscoveryBudget,
    defaultDirectoryTreeDiscoveryOptions,
    describeDirectoryError,
    discoverDirectory,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))

data DiscoverDirectoryArgs (fieldType :: FieldType) = DiscoverDirectoryArgs
  { path ::
      Field fieldType FilePath
        `WithMeta` '[ Description "Path of directory to discover, relative to the project root.",
                      Example ".",
                      Example "src",
                      Example "src/features"
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (DiscoverDirectoryArgs 'ValueType)

instance ToSchema (DiscoverDirectoryArgs 'MetadataType)

discoverDirectoryTool :: (MonadLore m) => SomeTool m
discoverDirectoryTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "discoverDirectory",
        description = Just "Discover and render a directory tree recursively. It has depth and item limiters to avoid bloating the context window.",
        handler = discoverDirectoryHandler
      }

discoverDirectoryHandler :: (MonadLore m) => DiscoverDirectoryArgs 'ValueType -> m Text
discoverDirectoryHandler DiscoverDirectoryArgs {path} = do
  discoveredTree <- discoverDirectory options
  pure $
    case discoveredTree of
      Left directoryError ->
        T.pack (describeDirectoryError directoryError)
      Right directoryTree ->
        renderDirectoryTree directoryTree
  where
    options =
      defaultDirectoryTreeDiscoveryOptions
        { directoryTreeRootPath = path,
          directoryTreeFocusPaths = ["."],
          directoryTreeBudget = Just defaultDirectoryTreeDiscoveryBudget,
          directoryTreeNoisyDirectoryOptions =
            Just
              DirectoryTreeNoisyDirectoryOptions
                { directoryTreeNoisyDirectoryMinEntries = 20,
                  directoryTreeNoisyDirectoryHeadEntries = 3,
                  directoryTreeNoisyDirectoryTailEntries = 3
                }
        }

renderDirectoryTree :: DirectoryTree -> Text
renderDirectoryTree directoryTree =
  T.unlines $
    T.pack (renderDirectoryPath directoryTree.directoryTreeRootRelativePath)
      : renderChildren "" directoryTree.directoryTreeRoot.directoryTreeNodeChildren

renderDirectoryPath :: FilePath -> FilePath
renderDirectoryPath path
  | path == "." = "./"
  | otherwise = path <> "/"

renderChildren :: Text -> [DirectoryTreeChild] -> [Text]
renderChildren prefix children =
  concatMap renderChildWithIndex (zip [0 :: Int ..] children)
  where
    lastIndex = length children - 1

    renderChildWithIndex (index, child) =
      renderChild prefix (index == lastIndex) child

renderChild :: Text -> Bool -> DirectoryTreeChild -> [Text]
renderChild prefix isLast child =
  case child of
    DirectoryTreeChildFile file ->
      [prefix <> marker <> T.pack file.directoryTreeFileName]
    DirectoryTreeChildOmitted omitted ->
      [prefix <> marker <> T.pack (renderOmittedEntries omitted)]
    DirectoryTreeChildDirectory node ->
      (prefix <> marker <> renderNodeLabel node)
        : if node.directoryTreeNodeOpened
          then renderChildren (prefix <> childIndent) node.directoryTreeNodeChildren
          else []
  where
    marker =
      if isLast
        then "└── "
        else "├── "

    childIndent =
      if isLast
        then "    "
        else "│   "

renderNodeLabel :: DirectoryTreeNode -> Text
renderNodeLabel node =
  T.pack node.directoryTreeNodeName
    <> "/"
    <> directorySuffix node

directorySuffix :: DirectoryTreeNode -> Text
directorySuffix node
  | node.directoryTreeNodeOpened =
      case node.directoryTreeNodeInlineOmitted of
        Nothing -> ""
        Just omitted -> " (" <> T.pack (renderInlineOmittedEntries omitted) <> ")"
  | otherwise =
      closedDirectorySummary node.directoryTreeNodeStats

closedDirectorySummary :: Maybe DirectoryTreeStats -> Text
closedDirectorySummary maybeStats =
  case maybeStats of
    Nothing -> ""
    Just stats ->
      " (" <> T.pack (show stats.directoryTreeStatsTotalFiles) <> " files)"

renderInlineOmittedEntries :: DirectoryTreeOmittedEntries -> String
renderInlineOmittedEntries omitted =
  show omitted.directoryTreeOmittedDirectories
    <> " dirs, "
    <> show omitted.directoryTreeOmittedFiles
    <> " files"

renderOmittedEntries :: DirectoryTreeOmittedEntries -> String
renderOmittedEntries omitted =
  case nonEmptyParts of
    [] -> "... omitted"
    _ -> "... omitted: " <> intercalate ", " nonEmptyParts
  where
    nonEmptyParts =
      filter (not . null) [directoriesPart, filesPart]

    directoriesPart
      | omitted.directoryTreeOmittedDirectories > 0 =
          show omitted.directoryTreeOmittedDirectories <> " dirs"
      | otherwise = ""

    filesPart
      | omitted.directoryTreeOmittedFiles > 0 =
          show omitted.directoryTreeOmittedFiles <> " files"
      | otherwise = ""
