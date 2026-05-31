module Lore.Tools.DirectoryTree
  ( DirectoryTreeDiscoveryOptions (..),
    DirectoryTreeNoisyDirectoryOptions (..),
    defaultDirectoryTreeDiscoveryOptions,
    DirectoryTree (..),
    DirectoryTreeNode (..),
    DirectoryTreeChild (..),
    DirectoryTreeFile (..),
    DirectoryTreeStats (..),
    DirectoryTreeExtensionStats (..),
    DirectoryTreeOmittedEntries (..),
    DirectoryError (..),
    describeDirectoryError,
    discoverDirectory,
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Data.Char (toLower)
import Data.List (elemIndex, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Lore (MonadLore)
import Lore.Tools.Directory
  ( DirectoryEntry (..),
    DirectoryEntryType (..),
    DirectoryError (..),
    appendRelativePath,
    describeDirectoryError,
    displayRelativePath,
    isAncestorOrSelf,
    isInsideRelativePath,
    listVisibleDirectoryEntries,
    normalizeRelativePath,
    relativeProjectPath,
    resolveDirectoryInsideProject,
  )
import Lore.Tools.DirectoryTree.Gitignore
  ( GitignoredDirectoryMatcher,
    isHiddenDirectoryPath,
    loadGitignoredDirectoryMatchers,
    matchesGitignoredDirectory,
  )
import Lore.Session (SessionContext (..))
import System.Directory (canonicalizePath)
import System.FilePath
  ( isAbsolute,
    makeRelative,
    normalise,
    splitDirectories,
    takeExtension,
    (</>),
  )

data DirectoryTreeDiscoveryOptions = DirectoryTreeDiscoveryOptions
  { -- | Directory from which tree discovery starts.
    --
    -- Relative paths are resolved from the project root.
    directoryTreeRootPath :: FilePath,
    -- | Paths around which the tree should be expanded.
    --
    -- Paths may be absolute or relative to 'directoryTreeRootPath'.
    --
    -- A directory is opened when it is:
    --
    --   * the root;
    --   * one of the focus paths;
    --   * inside one of the focus paths;
    --   * an ancestor of one of the focus paths.
    directoryTreeFocusPaths :: [FilePath],
    -- | Budget for rendered file and directory entries.
    --
    -- The root node itself does not count.
    --
    -- A compressed path such as @foo/bar/baz/@ counts as one rendered entry.
    --
    -- Nothing means the default budget.
    directoryTreeBudget :: Maybe Int,
    -- | Optional maximum directory depth relative to the requested root.
    --
    -- Depth @0@ means render only the requested root.
    -- Depth @1@ includes only direct children.
    directoryTreeDepth :: Maybe Int,
    -- | Optional strategy for trimming noisy opened directories.
    --
    -- The root node is never treated as noisy.
    directoryTreeNoisyDirectoryOptions :: Maybe DirectoryTreeNoisyDirectoryOptions
  }
  deriving stock (Eq, Show)

data DirectoryTreeNoisyDirectoryOptions = DirectoryTreeNoisyDirectoryOptions
  { -- | Minimum number of immediate entries to treat the directory as noisy.
    directoryTreeNoisyDirectoryMinEntries :: Int,
    -- | Number of entries to keep from the beginning.
    directoryTreeNoisyDirectoryHeadEntries :: Int,
    -- | Number of entries to keep from the end.
    directoryTreeNoisyDirectoryTailEntries :: Int
  }
  deriving stock (Eq, Show)

defaultDirectoryTreeDiscoveryOptions :: DirectoryTreeDiscoveryOptions
defaultDirectoryTreeDiscoveryOptions =
  DirectoryTreeDiscoveryOptions
    { directoryTreeRootPath = ".",
      directoryTreeFocusPaths = [],
      directoryTreeBudget = Nothing,
      directoryTreeDepth = Nothing,
      directoryTreeNoisyDirectoryOptions = Nothing
    }

data DirectoryTree = DirectoryTree
  { directoryTreeRootPathResolved :: FilePath,
    directoryTreeRootRelativePath :: FilePath,
    -- | Focus paths normalized relative to the tree root.
    directoryTreeFocusPathsUsed :: [FilePath],
    -- | Effective budget used for rendered file and directory entries, if any.
    directoryTreeBudgetUsed :: Maybe Int,
    directoryTreeRoot :: DirectoryTreeNode
  }
  deriving stock (Eq, Show)

data DirectoryTreeNode = DirectoryTreeNode
  { -- | Display name relative to the parent node.
    --
    -- This can be a compressed path, for example @foo/bar/baz@.
    directoryTreeNodeName :: FilePath,
    -- | Real target path relative to the project root display path.
    directoryTreeNodeRelativePath :: FilePath,
    directoryTreeNodeOpened :: Bool,
    -- | Present only for rendered directories that were not opened.
    directoryTreeNodeStats :: Maybe DirectoryTreeStats,
    -- | Present for opened directories when omission should be rendered inline.
    directoryTreeNodeInlineOmitted :: Maybe DirectoryTreeOmittedEntries,
    directoryTreeNodeChildren :: [DirectoryTreeChild]
  }
  deriving stock (Eq, Show)

data DirectoryTreeChild
  = DirectoryTreeChildDirectory DirectoryTreeNode
  | DirectoryTreeChildFile DirectoryTreeFile
  | DirectoryTreeChildOmitted DirectoryTreeOmittedEntries
  deriving stock (Eq, Show)

data DirectoryTreeFile = DirectoryTreeFile
  { -- | Display name relative to the parent node.
    --
    -- This can be a compressed path, for example @foo/bar/File.hs@.
    directoryTreeFileName :: FilePath,
    directoryTreeFileRelativePath :: FilePath
  }
  deriving stock (Eq, Show)

data DirectoryTreeStats = DirectoryTreeStats
  { directoryTreeStatsTotalFiles :: Int,
    directoryTreeStatsFilesByExtension :: [DirectoryTreeExtensionStats]
  }
  deriving stock (Eq, Show)

data DirectoryTreeExtensionStats = DirectoryTreeExtensionStats
  { directoryTreeExtensionStatsExtension :: Maybe FilePath,
    directoryTreeExtensionStatsFileCount :: Int
  }
  deriving stock (Eq, Show)

data DirectoryTreeOmittedEntries = DirectoryTreeOmittedEntries
  { directoryTreeOmittedDirectories :: Int,
    directoryTreeOmittedFiles :: Int
  }
  deriving stock (Eq, Show)

instance Semigroup DirectoryTreeOmittedEntries where
  left <> right =
    DirectoryTreeOmittedEntries
      { directoryTreeOmittedDirectories =
          left.directoryTreeOmittedDirectories + right.directoryTreeOmittedDirectories,
        directoryTreeOmittedFiles =
          left.directoryTreeOmittedFiles + right.directoryTreeOmittedFiles
      }

instance Monoid DirectoryTreeOmittedEntries where
  mempty =
    DirectoryTreeOmittedEntries
      { directoryTreeOmittedDirectories = 0,
        directoryTreeOmittedFiles = 0
      }

discoverDirectory ::
  (MonadLore m) =>
  DirectoryTreeDiscoveryOptions ->
  m (Either DirectoryError DirectoryTree)
discoverDirectory options = do
  projectRootPath <- asks projectRoot
  canonicalProjectRoot <- liftIO $ canonicalizePath projectRootPath

  resolvedRoot <-
    liftIO $
      resolveDirectoryInsideProject
        projectRootPath
        (directoryTreeRootPath options)

  case resolvedRoot of
    Left err -> pure (Left err)
    Right rootPath -> do
      let budget = discoveryBudget options
          maxDepth = discoveryDepth options
          focusPaths = normalizeFocusPaths rootPath (directoryTreeFocusPaths options)
          focusPathList = Set.toAscList focusPaths
          noisyOptions = normalizeNoisyDirectoryOptions (directoryTreeNoisyDirectoryOptions options)
          rootRelativePath = relativeProjectPath canonicalProjectRoot rootPath

      gitignoredMatchers <- liftIO $ loadGitignoredDirectoryMatchers canonicalProjectRoot

      plan <-
        liftIO $
          planDirectoryTree
            budget
            maxDepth
            rootPath
            focusPaths
            noisyOptions
            gitignoredMatchers

      let treeRoot = materializeDirectory rootRelativePath "." "." plan

      pure $
        Right
          DirectoryTree
            { directoryTreeRootPathResolved = rootPath,
              directoryTreeRootRelativePath = rootRelativePath,
              directoryTreeFocusPathsUsed = focusPathList,
              directoryTreeBudgetUsed = budget,
              directoryTreeRoot = treeRoot
            }

discoveryBudget :: DirectoryTreeDiscoveryOptions -> Maybe Int
discoveryBudget options =
  fmap (max 0) options.directoryTreeBudget

discoveryDepth :: DirectoryTreeDiscoveryOptions -> Maybe Int
discoveryDepth options =
  fmap (max 0) options.directoryTreeDepth

data PlannedDirectory
  = PlannedOpenedDirectory [PlannedEntry] PlannedDirectoryOmission
  | PlannedClosedDirectory DirectoryTreeStats
  deriving stock (Eq, Show)

data PlannedDirectoryOmission
  = PlannedDirectoryNoOmission
  | PlannedDirectoryInlineOmission DirectoryTreeOmittedEntries
  | PlannedDirectoryChildOmission Int DirectoryTreeOmittedEntries
  deriving stock (Eq, Show)

data PlannedEntry
  = PlannedEntryDirectory PlannedDirectoryEntry
  | PlannedEntryFile PlannedFileEntry
  deriving stock (Eq, Show)

data PlannedDirectoryEntry = PlannedDirectoryEntry
  { plannedDirectoryEntryDisplayName :: FilePath,
    plannedDirectoryEntryRelativePath :: FilePath
  }
  deriving stock (Eq, Show)

data PlannedFileEntry = PlannedFileEntry
  { plannedFileEntryDisplayName :: FilePath,
    plannedFileEntryRelativePath :: FilePath
  }
  deriving stock (Eq, Show)

type DirectoryPlan = Map.Map FilePath PlannedDirectory

data PlannedChildDirectory
  = PlannedChildDirectoryOpened FilePath
  | PlannedChildDirectoryClosed FilePath DirectoryTreeStats
  deriving stock (Eq, Show)

planDirectoryTree ::
  Maybe Int ->
  Maybe Int ->
  FilePath ->
  Set.Set FilePath ->
  Maybe DirectoryTreeNoisyDirectoryOptions ->
  [GitignoredDirectoryMatcher] ->
  IO DirectoryPlan
planDirectoryTree budget maybeMaxDepth rootPath focusPaths noisyOptions gitignoredMatchers =
  go Set.empty budget ["."] Map.empty
  where
    go _ _ [] plan = pure plan
    go visited maybeRemaining (relativePath : queue) plan = do
      canonicalDirectoryPath <- canonicalizePath (rootPath </> relativePath)

      if canonicalDirectoryPath `Set.member` visited
        then do
          plan' <- ensureClosedDirectoryPlan relativePath plan
          go visited maybeRemaining queue plan'
        else do
          rawEntries <- listVisibleDirectoryEntries (rootPath </> relativePath)
          let noisySelection =
                selectNoisyEntries noisyOptions relativePath rawEntries
          entries <-
            case maybeMaxDepth of
              Nothing ->
                forM noisySelection.selectedEntries $
                  collapsePlainEntry rootPath relativePath
              Just _ ->
                pure $
                  map
                    (directPlannedEntry relativePath)
                    noisySelection.selectedEntries

          let prioritizedEntries =
                if isNoisySelectionTrimmed noisySelection
                  then entries
                  else prioritizePlannedEntries focusPaths entries

              (renderedEntries, omittedEntries, maybeRemaining') =
                case maybeRemaining of
                  Nothing ->
                    (prioritizedEntries, [], Nothing)
                  Just remaining ->
                    let (renderedEntries', omittedEntries') = splitAt remaining prioritizedEntries
                     in (renderedEntries', omittedEntries', Just (remaining - length renderedEntries'))
              noisyOmitted = countDirectoryEntries noisySelection.omittedEntries
              budgetOmitted = countPlannedEntries omittedEntries
              omission =
                planDirectoryOmission
                  relativePath
                  noisySelection
                  renderedEntries
                  noisyOmitted
                  (noisyOmitted <> budgetOmitted)

              renderedDirectories =
                [ directoryEntry
                | PlannedEntryDirectory directoryEntry <- renderedEntries
                ]

          childDirectoryPlans <-
            forM renderedDirectories \directoryEntry -> do
              let childRelativePath =
                    plannedDirectoryEntryRelativePath directoryEntry
                  shouldOpenForDepth =
                    maybe True (directoryDepth childRelativePath <=) maybeMaxDepth

              if shouldOpenForDepth
                && shouldOpenDirectory
                  focusPaths
                  childRelativePath
                  gitignoredMatchers
                then pure (PlannedChildDirectoryOpened childRelativePath)
                else do
                  stats <- summarizeDirectory (rootPath </> childRelativePath)
                  pure $
                    PlannedChildDirectoryClosed childRelativePath stats

          let openedDirectoryPaths =
                [ childRelativePath
                | PlannedChildDirectoryOpened childRelativePath <- childDirectoryPlans
                ]

              currentPlan =
                PlannedOpenedDirectory renderedEntries omission

              plan' =
                foldr
                  insertChildDirectoryPlan
                  (Map.insert relativePath currentPlan plan)
                  childDirectoryPlans

              visited' = Set.insert canonicalDirectoryPath visited

          go visited' maybeRemaining' (queue <> openedDirectoryPaths) plan'
    ensureClosedDirectoryPlan relativePath plan
      | Map.member relativePath plan = pure plan
      | otherwise = do
          stats <- summarizeDirectory (rootPath </> relativePath)
          pure $
            Map.insert
              relativePath
              (PlannedClosedDirectory stats)
              plan

directoryDepth :: FilePath -> Int
directoryDepth relativePath =
  case normalizeRelativePath relativePath of
    "." ->
      0
    normalizedPath ->
      length $
        filter
          (\segment -> segment /= "." && segment /= "")
          (splitDirectories normalizedPath)

insertChildDirectoryPlan :: PlannedChildDirectory -> DirectoryPlan -> DirectoryPlan
insertChildDirectoryPlan = \case
  PlannedChildDirectoryOpened _ ->
    id
  PlannedChildDirectoryClosed childRelativePath stats ->
    Map.insert
      childRelativePath
      (PlannedClosedDirectory stats)

planDirectoryOmission ::
  FilePath ->
  NoisySelection ->
  [PlannedEntry] ->
  DirectoryTreeOmittedEntries ->
  DirectoryTreeOmittedEntries ->
  PlannedDirectoryOmission
planDirectoryOmission relativePath noisySelection renderedEntries noisyOmitted omitted
  | isEmptyOmittedEntries omitted = PlannedDirectoryNoOmission
  | shouldKeepOmittedAsChild =
      PlannedDirectoryChildOmission omittedInsertAt omitted
  | otherwise =
      PlannedDirectoryInlineOmission omitted
  where
    omittedInsertAt =
      min
        (length renderedEntries)
        noisySelection.selectedHeadEntriesCount

    shouldKeepOmittedAsChild =
      relativePath == "."
        || (not (isEmptyOmittedEntries noisyOmitted) && not (null renderedEntries))

-- | Collapse a raw filesystem entry into one rendered entry.
--
-- A rendered entry can represent a whole non-branching path.
--
-- Examples:
--
--   * @foo/@ remains a directory when @foo/@ is empty or branching.
--   * @foo/bar/baz/@ becomes one directory entry when every intermediate
--     directory has exactly one visible child directory.
--   * @foo/bar/File.hs@ becomes one file entry when the chain ends in a
--     single visible file.
collapsePlainEntry ::
  FilePath ->
  FilePath ->
  DirectoryEntry ->
  IO PlannedEntry
collapsePlainEntry rootPath parentRelativePath entry =
  case directoryEntryType entry of
    DirectoryEntryFile ->
      pure $
        PlannedEntryFile
          PlannedFileEntry
            { plannedFileEntryDisplayName = directoryEntryName entry,
              plannedFileEntryRelativePath =
                appendRelativePath parentRelativePath (directoryEntryName entry)
            }
    DirectoryEntryDirectory ->
      collapsePlainDirectoryChain
        rootPath
        (directoryEntryName entry)
        (appendRelativePath parentRelativePath (directoryEntryName entry))

directPlannedEntry :: FilePath -> DirectoryEntry -> PlannedEntry
directPlannedEntry parentRelativePath entry =
  case directoryEntryType entry of
    DirectoryEntryFile ->
      PlannedEntryFile
        PlannedFileEntry
          { plannedFileEntryDisplayName = directoryEntryName entry,
            plannedFileEntryRelativePath =
              appendRelativePath parentRelativePath (directoryEntryName entry)
          }
    DirectoryEntryDirectory ->
      PlannedEntryDirectory
        PlannedDirectoryEntry
          { plannedDirectoryEntryDisplayName = directoryEntryName entry,
            plannedDirectoryEntryRelativePath =
              appendRelativePath parentRelativePath (directoryEntryName entry)
          }

collapsePlainDirectoryChain ::
  FilePath ->
  FilePath ->
  FilePath ->
  IO PlannedEntry
collapsePlainDirectoryChain rootPath displayName relativePath =
  go Set.empty displayName relativePath
  where
    go visited currentDisplayName currentRelativePath = do
      canonicalDirectoryPath <- canonicalizePath (rootPath </> currentRelativePath)

      if canonicalDirectoryPath `Set.member` visited
        then pureDirectory currentDisplayName currentRelativePath
        else do
          entries <- listVisibleDirectoryEntries (rootPath </> currentRelativePath)

          case entries of
            [singleEntry] ->
              case directoryEntryType singleEntry of
                DirectoryEntryDirectory ->
                  go
                    (Set.insert canonicalDirectoryPath visited)
                    (currentDisplayName </> directoryEntryName singleEntry)
                    (appendRelativePath currentRelativePath (directoryEntryName singleEntry))
                DirectoryEntryFile ->
                  pure $
                    PlannedEntryFile
                      PlannedFileEntry
                        { plannedFileEntryDisplayName =
                            currentDisplayName </> directoryEntryName singleEntry,
                          plannedFileEntryRelativePath =
                            appendRelativePath currentRelativePath (directoryEntryName singleEntry)
                        }
            _ ->
              pureDirectory currentDisplayName currentRelativePath

    pureDirectory currentDisplayName currentRelativePath =
      pure $
        PlannedEntryDirectory
          PlannedDirectoryEntry
            { plannedDirectoryEntryDisplayName = currentDisplayName,
              plannedDirectoryEntryRelativePath = currentRelativePath
            }

materializeDirectory ::
  FilePath ->
  FilePath ->
  FilePath ->
  DirectoryPlan ->
  DirectoryTreeNode
materializeDirectory rootRelativePath displayName relativePath plan =
  case Map.lookup relativePath plan of
    Nothing ->
      missingDirectoryPlan relativePath
    Just planned ->
      case planned of
        PlannedClosedDirectory stats ->
          directoryNode
            False
            (Just stats)
            Nothing
            []
        PlannedOpenedDirectory plannedDirectoryEntries plannedDirectoryOmission ->
          let renderedChildren =
                map materializeChild plannedDirectoryEntries
              (nodeInlineOmitted, nodeChildren) =
                materializeOmission plannedDirectoryOmission renderedChildren
           in directoryNode
                True
                Nothing
                nodeInlineOmitted
                nodeChildren
  where
    directoryNode opened stats nodeInlineOmitted children =
      DirectoryTreeNode
        { directoryTreeNodeName = displayName,
          directoryTreeNodeRelativePath = displayRelativePath rootRelativePath relativePath,
          directoryTreeNodeOpened = opened,
          directoryTreeNodeStats = stats,
          directoryTreeNodeInlineOmitted = nodeInlineOmitted,
          directoryTreeNodeChildren = children
        }

    materializeChild = \case
      PlannedEntryDirectory directoryEntry ->
        DirectoryTreeChildDirectory $
          materializeDirectory
            rootRelativePath
            (plannedDirectoryEntryDisplayName directoryEntry)
            (plannedDirectoryEntryRelativePath directoryEntry)
            plan
      PlannedEntryFile fileEntry ->
        DirectoryTreeChildFile
          DirectoryTreeFile
            { directoryTreeFileName = plannedFileEntryDisplayName fileEntry,
              directoryTreeFileRelativePath =
                displayRelativePath
                  rootRelativePath
                  (plannedFileEntryRelativePath fileEntry)
            }

missingDirectoryPlan :: FilePath -> a
missingDirectoryPlan relativePath =
  error $
    "Internal directory-tree invariant violated: missing plan for "
      <> relativePath

materializeOmission ::
  PlannedDirectoryOmission ->
  [DirectoryTreeChild] ->
  (Maybe DirectoryTreeOmittedEntries, [DirectoryTreeChild])
materializeOmission omission children =
  case omission of
    PlannedDirectoryChildOmission insertAt omitted ->
      let safeInsertAt = max 0 (min insertAt (length children))
       in ( Nothing,
            take safeInsertAt children
              <> [DirectoryTreeChildOmitted omitted]
              <> drop safeInsertAt children
          )
    PlannedDirectoryNoOmission ->
      (Nothing, children)
    PlannedDirectoryInlineOmission omitted ->
      (Just omitted, children)

countPlannedEntries :: [PlannedEntry] -> DirectoryTreeOmittedEntries
countPlannedEntries =
  countOmittedEntries plannedEntryOmittedEntries

countDirectoryEntries :: [DirectoryEntry] -> DirectoryTreeOmittedEntries
countDirectoryEntries =
  countOmittedEntries directoryEntryOmittedEntries

countOmittedEntries :: (entry -> DirectoryTreeOmittedEntries) -> [entry] -> DirectoryTreeOmittedEntries
countOmittedEntries toOmittedEntries =
  mconcat . map toOmittedEntries

plannedEntryOmittedEntries :: PlannedEntry -> DirectoryTreeOmittedEntries
plannedEntryOmittedEntries = \case
  PlannedEntryDirectory _ ->
    oneOmittedDirectory
  PlannedEntryFile _ ->
    oneOmittedFile

directoryEntryOmittedEntries :: DirectoryEntry -> DirectoryTreeOmittedEntries
directoryEntryOmittedEntries entry =
  case directoryEntryType entry of
    DirectoryEntryDirectory ->
      oneOmittedDirectory
    DirectoryEntryFile ->
      oneOmittedFile

oneOmittedDirectory :: DirectoryTreeOmittedEntries
oneOmittedDirectory =
  mempty {directoryTreeOmittedDirectories = 1}

oneOmittedFile :: DirectoryTreeOmittedEntries
oneOmittedFile =
  mempty {directoryTreeOmittedFiles = 1}

isEmptyOmittedEntries :: DirectoryTreeOmittedEntries -> Bool
isEmptyOmittedEntries omitted =
  omitted.directoryTreeOmittedDirectories == 0
    && omitted.directoryTreeOmittedFiles == 0

data NoisySelection = NoisySelection
  { selectedEntries :: [DirectoryEntry],
    selectedHeadEntriesCount :: Int,
    omittedEntries :: [DirectoryEntry]
  }

isNoisySelectionTrimmed :: NoisySelection -> Bool
isNoisySelectionTrimmed selection =
  selection.selectedHeadEntriesCount < length selection.selectedEntries

prioritizePlannedEntries :: Set.Set FilePath -> [PlannedEntry] -> [PlannedEntry]
prioritizePlannedEntries focusPaths entries =
  sortBy (comparing (plannedEntryPriorityKey focusPathList)) entries
  where
    focusPathList =
      [ focusPath
      | focusPath <- Set.toList focusPaths,
        focusPath /= "."
      ]

plannedEntryPriorityKey :: [FilePath] -> PlannedEntry -> (Int, Int, Int)
plannedEntryPriorityKey focusPathList = \case
  PlannedEntryDirectory directoryEntry ->
    let normalizedPath =
          normalizeRelativePath
            (plannedDirectoryEntryRelativePath directoryEntry)
        focusPriority =
          if any (isFocusRelated normalizedPath) focusPathList
            then 0
            else 1
        namePriority =
          directoryOpenNamePriority
            (plannedDirectoryEntryPriorityName directoryEntry)
        categoryPriority =
          if namePriority == maxBound
            then 2
            else 1
     in ( focusPriority,
          categoryPriority,
          namePriority
        )
  PlannedEntryFile _ ->
    (4, 4, maxBound)
  where
    isFocusRelated normalizedPath focusPath =
      isAncestorOrSelf normalizedPath focusPath
        || isAncestorOrSelf focusPath normalizedPath

plannedDirectoryEntryPriorityName :: PlannedDirectoryEntry -> String
plannedDirectoryEntryPriorityName directoryEntry =
  map toLower $
    case splitDirectories (plannedDirectoryEntryDisplayName directoryEntry) of
      [] -> plannedDirectoryEntryDisplayName directoryEntry
      segment : _ -> segment

directoryOpenNamePriority :: String -> Int
directoryOpenNamePriority directoryName =
  fromMaybe
    maxBound
    (elemIndex directoryName prioritizedDirectoryNames)

prioritizedDirectoryNames :: [String]
prioritizedDirectoryNames =
  [ "src",
    "app",
    "lib",
    "migrations",
    "migration",
    "database",
    "db",
    "schema",
    "sql",
    "test",
    "tests",
    "spec",
    "bench",
    "examples",
    "scripts",
    "config"
  ]

selectNoisyEntries ::
  Maybe DirectoryTreeNoisyDirectoryOptions ->
  FilePath ->
  [DirectoryEntry] ->
  NoisySelection
selectNoisyEntries maybeNoisyOptions relativePath entries
  | relativePath == "." =
      NoisySelection {selectedEntries = entries, selectedHeadEntriesCount = length entries, omittedEntries = []}
  | otherwise =
      case maybeNoisyOptions of
        Nothing ->
          NoisySelection {selectedEntries = entries, selectedHeadEntriesCount = length entries, omittedEntries = []}
        Just noisyOptions ->
          if length entries < noisyOptions.directoryTreeNoisyDirectoryMinEntries
            then NoisySelection {selectedEntries = entries, selectedHeadEntriesCount = length entries, omittedEntries = []}
            else
              let totalEntries = length entries
                  headCount = min totalEntries noisyOptions.directoryTreeNoisyDirectoryHeadEntries
                  tailStart = max headCount (totalEntries - noisyOptions.directoryTreeNoisyDirectoryTailEntries)
                  selectedHead = take headCount entries
                  selectedTail = drop tailStart entries
                  omittedCount = tailStart - headCount
                  omittedMiddle = take omittedCount (drop headCount entries)
               in NoisySelection
                    { selectedEntries = selectedHead <> selectedTail,
                      selectedHeadEntriesCount = length selectedHead,
                      omittedEntries = omittedMiddle
                    }

normalizeNoisyDirectoryOptions ::
  Maybe DirectoryTreeNoisyDirectoryOptions ->
  Maybe DirectoryTreeNoisyDirectoryOptions
normalizeNoisyDirectoryOptions =
  fmap
    ( \options ->
        DirectoryTreeNoisyDirectoryOptions
          { directoryTreeNoisyDirectoryMinEntries = max 1 options.directoryTreeNoisyDirectoryMinEntries,
            directoryTreeNoisyDirectoryHeadEntries = max 0 options.directoryTreeNoisyDirectoryHeadEntries,
            directoryTreeNoisyDirectoryTailEntries = max 0 options.directoryTreeNoisyDirectoryTailEntries
          }
    )

shouldOpenDirectory ::
  Set.Set FilePath ->
  FilePath ->
  [GitignoredDirectoryMatcher] ->
  Bool
shouldOpenDirectory focusPaths relativePath gitignoredMatchers =
  normalizedPath == "."
    || (not isHiddenPath && not isGitignoredPath && any focusRelated (Set.toList focusPaths))
    || any explicitNoOpenByDefaultFocusRelated (Set.toList focusPaths)
  where
    normalizedPath = normalizeRelativePath relativePath
    isHiddenPath = isHiddenDirectoryPath normalizedPath
    isGitignoredPath = matchesGitignoredDirectory gitignoredMatchers normalizedPath

    focusRelated focusPath =
      isAncestorOrSelf normalizedPath focusPath
        || isAncestorOrSelf focusPath normalizedPath

    explicitNoOpenByDefaultFocusRelated focusPath =
      focusPath /= "."
        && (isHiddenDirectoryPath focusPath || matchesGitignoredDirectory gitignoredMatchers focusPath)
        && focusRelated focusPath

summarizeDirectory :: FilePath -> IO DirectoryTreeStats
summarizeDirectory directoryPath = do
  (totalFiles, filesByExtension) <- summarizeDirectoryMap Set.empty directoryPath
  pure
    DirectoryTreeStats
      { directoryTreeStatsTotalFiles = totalFiles,
        directoryTreeStatsFilesByExtension = extensionStatsFromMap filesByExtension
      }

summarizeDirectoryMap ::
  Set.Set FilePath ->
  FilePath ->
  IO (Int, Map.Map (Maybe FilePath) Int)
summarizeDirectoryMap visited directoryPath = do
  canonicalDirectoryPath <- canonicalizePath directoryPath

  if canonicalDirectoryPath `Set.member` visited
    then pure (0, Map.empty)
    else do
      entries <- listVisibleDirectoryEntries directoryPath

      let directories =
            [ entry
            | entry <- entries,
              directoryEntryType entry == DirectoryEntryDirectory
            ]

          files =
            [ entry
            | entry <- entries,
              directoryEntryType entry == DirectoryEntryFile
            ]

          directFilesByExtension =
            Map.fromListWith
              (+)
              [ (extensionKey (directoryEntryName entry), 1)
              | entry <- files
              ]

          visited' = Set.insert canonicalDirectoryPath visited

      childSummaries <-
        forM directories \entry ->
          summarizeDirectoryMap visited' (directoryPath </> directoryEntryName entry)

      let childFileCount = sum (map fst childSummaries)
          childFilesByExtension = Map.unionsWith (+) (map snd childSummaries)

      pure
        ( length files + childFileCount,
          Map.unionWith (+) directFilesByExtension childFilesByExtension
        )

extensionStatsFromMap :: Map.Map (Maybe FilePath) Int -> [DirectoryTreeExtensionStats]
extensionStatsFromMap filesByExtension =
  [ DirectoryTreeExtensionStats
      { directoryTreeExtensionStatsExtension = extension,
        directoryTreeExtensionStatsFileCount = fileCount
      }
  | (extension, fileCount) <- Map.toAscList filesByExtension
  ]

extensionKey :: FilePath -> Maybe FilePath
extensionKey path =
  case takeExtension path of
    "" -> Nothing
    extension -> Just extension

normalizeFocusPaths :: FilePath -> [FilePath] -> Set.Set FilePath
normalizeFocusPaths rootPath focusPaths =
  Set.fromList $
    mapMaybe (normalizeFocusPath rootPath) focusPaths

normalizeFocusPath :: FilePath -> FilePath -> Maybe FilePath
normalizeFocusPath rootPath focusPath =
  let normalizedRoot = normalise rootPath
      absoluteFocusPath =
        normalise $
          if isAbsolute focusPath
            then focusPath
            else rootPath </> focusPath
      relativeFocusPath =
        normalizeRelativePath $
          makeRelative normalizedRoot absoluteFocusPath
   in if isInsideRelativePath relativeFocusPath
        then Just relativeFocusPath
        else Nothing
