module Lore.Tools.DiscoverDirectory
  ( DiscoverDirectoryOptions (..),
    DiscoverDirectoryOutput (..),
    DiscoverDirectoryRenderMode (..),
    discoverDirectory,
    renderDiscoverDirectory,
  )
where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Lore (MonadLore)
import Lore.Tools.DirectoryTree
  ( DirectoryTree (..),
    DirectoryTreeChild (..),
    DirectoryTreeDiscoveryOptions (..),
    DirectoryTreeFile (..),
    DirectoryTreeNode (..),
    DirectoryTreeNoisyDirectoryOptions (..),
    DirectoryTreeOmittedEntries (..),
    DirectoryTreeStats (..),
    defaultDirectoryTreeDiscoveryOptions,
    describeDirectoryError,
  )
import qualified Lore.Tools.DirectoryTree as DirectoryTree
import Lore.Tools.Render.Doc (LoreDoc, paragraph)

data DiscoverDirectoryOptions = DiscoverDirectoryOptions
  { discoverDirectoryPath :: FilePath,
    discoverDirectoryDepth :: Maybe Int,
    discoverDirectoryBudget :: Maybe Int
  }
  deriving stock (Eq, Show)

data DiscoverDirectoryOutput
  = DiscoverDirectoryFailed Text
  | DiscoverDirectoryReady DirectoryTree

data DiscoverDirectoryRenderMode
  = DiscoverDirectoryRenderTree
  | DiscoverDirectoryRenderCompact

discoverDirectory :: (MonadLore m) => DiscoverDirectoryOptions -> m DiscoverDirectoryOutput
discoverDirectory options = do
  discoveredTree <- DirectoryTree.discoverDirectory discoveryOptions
  pure $
    case discoveredTree of
      Left directoryError ->
        DiscoverDirectoryFailed (T.pack (describeDirectoryError directoryError))
      Right directoryTree ->
        DiscoverDirectoryReady directoryTree
  where
    discoveryOptions :: DirectoryTreeDiscoveryOptions
    discoveryOptions =
      defaultDirectoryTreeDiscoveryOptions
        { directoryTreeRootPath = options.discoverDirectoryPath,
          directoryTreeFocusPaths = ["."],
          directoryTreeBudget = options.discoverDirectoryBudget,
          directoryTreeDepth = fmap (max 0) options.discoverDirectoryDepth,
          directoryTreeNoisyDirectoryOptions =
            Just
              DirectoryTreeNoisyDirectoryOptions
                { directoryTreeNoisyDirectoryMinEntries = 20,
                  directoryTreeNoisyDirectoryHeadEntries = 3,
                  directoryTreeNoisyDirectoryTailEntries = 3
                }
        }

renderDiscoverDirectory :: DiscoverDirectoryRenderMode -> DiscoverDirectoryOutput -> LoreDoc
renderDiscoverDirectory renderMode output =
  case output of
    DiscoverDirectoryFailed message ->
      paragraph message
    DiscoverDirectoryReady tree ->
      paragraph $
        case renderMode of
          DiscoverDirectoryRenderTree -> renderDirectoryTree tree
          DiscoverDirectoryRenderCompact -> renderDirectoryTreeCompact tree

renderDirectoryTree :: DirectoryTree -> Text
renderDirectoryTree directoryTree =
  T.unlines $
    T.pack (renderDirectoryPath directoryTree.directoryTreeRootRelativePath)
      : renderChildren "" directoryTree.directoryTreeRoot.directoryTreeNodeChildren

renderDirectoryTreeCompact :: DirectoryTree -> Text
renderDirectoryTreeCompact directoryTree =
  T.unlines $
    concatMap renderChildCompact directoryTree.directoryTreeRoot.directoryTreeNodeChildren

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

renderChildCompact :: DirectoryTreeChild -> [Text]
renderChildCompact child =
  case child of
    DirectoryTreeChildFile file ->
      [T.pack file.directoryTreeFileName]
    DirectoryTreeChildOmitted omitted ->
      [T.pack (renderOmittedEntries omitted)]
    DirectoryTreeChildDirectory node ->
      T.pack (renderDirectoryPath node.directoryTreeNodeName)
        : if node.directoryTreeNodeOpened
          then concatMap renderChildCompact node.directoryTreeNodeChildren
          else []

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
