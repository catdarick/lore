module Lore.Mcp.Internal.DirectoryTree
  ( DirectoryTreeDiscoveryOptions (..),
    DirectoryTreeNoisyDirectoryOptions (..),
    defaultDirectoryTreeDiscoveryOptions,
    defaultDirectoryTreeDiscoveryBudget,
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
import Data.Char (isSpace, toLower)
import Data.List (elemIndex, isPrefixOf, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Lore (MonadLore)
import Lore.Mcp.Internal.Directory
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
import Lore.Session (SessionContext (..))
import System.Directory (canonicalizePath, doesFileExist)
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

defaultDirectoryTreeDiscoveryBudget :: Int
defaultDirectoryTreeDiscoveryBudget = 150

defaultDirectoryTreeDiscoveryOptions :: DirectoryTreeDiscoveryOptions
defaultDirectoryTreeDiscoveryOptions =
  DirectoryTreeDiscoveryOptions
    { directoryTreeRootPath = ".",
      directoryTreeFocusPaths = [],
      directoryTreeBudget = Just defaultDirectoryTreeDiscoveryBudget,
      directoryTreeNoisyDirectoryOptions = Nothing
    }

data DirectoryTree = DirectoryTree
  { directoryTreeRootPathResolved :: FilePath,
    directoryTreeRootRelativePath :: FilePath,
    -- | Focus paths normalized relative to the tree root.
    directoryTreeFocusPathsUsed :: [FilePath],
    -- | Effective budget used for rendered file and directory entries.
    directoryTreeBudgetUsed :: Int,
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
          focusPaths = normalizeFocusPaths rootPath (directoryTreeFocusPaths options)
          focusPathList = Set.toAscList focusPaths
          noisyOptions = normalizeNoisyDirectoryOptions (directoryTreeNoisyDirectoryOptions options)
          rootRelativePath = relativeProjectPath canonicalProjectRoot rootPath

      gitignoredMatchers <- liftIO $ loadGitignoredDirectoryMatchers canonicalProjectRoot

      plan <-
        liftIO $
          planDirectoryTree
            budget
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

discoveryBudget :: DirectoryTreeDiscoveryOptions -> Int
discoveryBudget options =
  max 0 $
    fromMaybe
      defaultDirectoryTreeDiscoveryBudget
      (directoryTreeBudget options)

data PlannedDirectory = PlannedDirectory
  { plannedDirectoryOpened :: Bool,
    plannedDirectoryEntries :: [PlannedEntry],
    plannedDirectoryOmittedInsertAt :: Int,
    plannedDirectoryInlineOmitted :: DirectoryTreeOmittedEntries,
    plannedDirectoryStats :: Maybe DirectoryTreeStats,
    plannedDirectoryChildOmitted :: DirectoryTreeOmittedEntries
  }
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

planDirectoryTree ::
  Int ->
  FilePath ->
  Set.Set FilePath ->
  Maybe DirectoryTreeNoisyDirectoryOptions ->
  [GitignoredDirectoryMatcher] ->
  IO DirectoryPlan
planDirectoryTree budget rootPath focusPaths noisyOptions gitignoredMatchers =
  go Set.empty (max 0 budget) ["."] Map.empty
  where
    go _ _ [] plan = pure plan
    go visited remaining (relativePath : queue) plan = do
      canonicalDirectoryPath <- canonicalizePath (rootPath </> relativePath)

      if canonicalDirectoryPath `Set.member` visited
        then go visited remaining queue plan
        else do
          rawEntries <- listVisibleDirectoryEntries (rootPath </> relativePath)
          let noisySelection =
                selectNoisyEntries noisyOptions relativePath rawEntries
          entries <-
            forM noisySelection.selectedEntries $
              collapsePlainEntry rootPath relativePath

          let prioritizedEntries =
                if isNoisySelectionTrimmed noisySelection
                  then entries
                  else prioritizePlannedEntries focusPaths entries

              (renderedEntries, omittedEntries) = splitAt remaining prioritizedEntries
              remaining' = remaining - length renderedEntries
              noisyOmitted = countDirectoryEntries noisySelection.omittedEntries
              budgetOmitted = countPlannedEntries omittedEntries
              omitted =
                mergeOmittedEntries noisyOmitted budgetOmitted
              omittedInsertAt =
                min
                  (length renderedEntries)
                  noisySelection.selectedHeadEntriesCount
              shouldKeepOmittedAsChild =
                relativePath == "."
                  || (not (isEmptyOmittedEntries noisyOmitted) && not (null renderedEntries))
              (inlineOmitted, childOmitted) =
                if shouldKeepOmittedAsChild
                  then (emptyOmittedEntries, omitted)
                  else (omitted, emptyOmittedEntries)

              renderedDirectories =
                [ directoryEntry
                | PlannedEntryDirectory directoryEntry <- renderedEntries
                ]

          closedDirectoryPlans <-
            forM renderedDirectories \directoryEntry -> do
              let childRelativePath =
                    plannedDirectoryEntryRelativePath directoryEntry

              if shouldOpenDirectory
                focusPaths
                childRelativePath
                gitignoredMatchers
                then pure Nothing
                else do
                  stats <- summarizeDirectory (rootPath </> childRelativePath)
                  pure $
                    Just
                      ( childRelativePath,
                        PlannedDirectory
                          { plannedDirectoryOpened = False,
                            plannedDirectoryEntries = [],
                            plannedDirectoryOmittedInsertAt = 0,
                            plannedDirectoryInlineOmitted = emptyOmittedEntries,
                            plannedDirectoryStats = Just stats,
                            plannedDirectoryChildOmitted = emptyOmittedEntries
                          }
                      )

          let openedDirectoryPaths =
                [ childRelativePath
                | directoryEntry <- renderedDirectories,
                  let childRelativePath =
                        plannedDirectoryEntryRelativePath directoryEntry,
                  shouldOpenDirectory
                    focusPaths
                    childRelativePath
                    gitignoredMatchers
                ]

              currentPlan =
                PlannedDirectory
                  { plannedDirectoryOpened = True,
                    plannedDirectoryEntries = renderedEntries,
                    plannedDirectoryOmittedInsertAt = omittedInsertAt,
                    plannedDirectoryInlineOmitted = inlineOmitted,
                    plannedDirectoryStats = Nothing,
                    plannedDirectoryChildOmitted = childOmitted
                  }

              plan' =
                foldr
                  (uncurry Map.insert)
                  (Map.insert relativePath currentPlan plan)
                  (mapMaybe id closedDirectoryPlans)

              visited' = Set.insert canonicalDirectoryPath visited

          go visited' remaining' (queue <> openedDirectoryPaths) plan'

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
  DirectoryTreeNode
    { directoryTreeNodeName = displayName,
      directoryTreeNodeRelativePath = displayRelativePath rootRelativePath relativePath,
      directoryTreeNodeOpened = plannedDirectoryOpened planned,
      directoryTreeNodeStats = plannedDirectoryStats planned,
      directoryTreeNodeInlineOmitted =
        toNonEmptyOmittedEntries planned.plannedDirectoryInlineOmitted,
      directoryTreeNodeChildren =
        if plannedDirectoryOpened planned
          then
            take omittedInsertAt renderedChildren
              <> omittedChildren
              <> drop omittedInsertAt renderedChildren
          else []
    }
  where
    planned =
      Map.findWithDefault
        fallbackClosedDirectory
        relativePath
        plan

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

    renderedChildren =
      map materializeChild (plannedDirectoryEntries planned)

    omittedInsertAt =
      max 0 (min planned.plannedDirectoryOmittedInsertAt (length renderedChildren))

    omitted = plannedDirectoryChildOmitted planned

    omittedChildren =
      [ DirectoryTreeChildOmitted omitted
      | directoryTreeOmittedDirectories omitted > 0
          || directoryTreeOmittedFiles omitted > 0
      ]

fallbackClosedDirectory :: PlannedDirectory
fallbackClosedDirectory =
  PlannedDirectory
    { plannedDirectoryOpened = False,
      plannedDirectoryEntries = [],
      plannedDirectoryOmittedInsertAt = 0,
      plannedDirectoryInlineOmitted = emptyOmittedEntries,
      plannedDirectoryStats = Nothing,
      plannedDirectoryChildOmitted = emptyOmittedEntries
    }

emptyOmittedEntries :: DirectoryTreeOmittedEntries
emptyOmittedEntries =
  DirectoryTreeOmittedEntries
    { directoryTreeOmittedDirectories = 0,
      directoryTreeOmittedFiles = 0
    }

countPlannedEntries :: [PlannedEntry] -> DirectoryTreeOmittedEntries
countPlannedEntries entries =
  DirectoryTreeOmittedEntries
    { directoryTreeOmittedDirectories =
        length
          [ ()
          | PlannedEntryDirectory _ <- entries
          ],
      directoryTreeOmittedFiles =
        length
          [ ()
          | PlannedEntryFile _ <- entries
          ]
    }

countDirectoryEntries :: [DirectoryEntry] -> DirectoryTreeOmittedEntries
countDirectoryEntries entries =
  DirectoryTreeOmittedEntries
    { directoryTreeOmittedDirectories =
        length
          [ ()
          | DirectoryEntry {directoryEntryType = DirectoryEntryDirectory} <- entries
          ],
      directoryTreeOmittedFiles =
        length
          [ ()
          | DirectoryEntry {directoryEntryType = DirectoryEntryFile} <- entries
          ]
    }

mergeOmittedEntries :: DirectoryTreeOmittedEntries -> DirectoryTreeOmittedEntries -> DirectoryTreeOmittedEntries
mergeOmittedEntries left right =
  DirectoryTreeOmittedEntries
    { directoryTreeOmittedDirectories =
        left.directoryTreeOmittedDirectories + right.directoryTreeOmittedDirectories,
      directoryTreeOmittedFiles =
        left.directoryTreeOmittedFiles + right.directoryTreeOmittedFiles
    }

toNonEmptyOmittedEntries :: DirectoryTreeOmittedEntries -> Maybe DirectoryTreeOmittedEntries
toNonEmptyOmittedEntries omitted
  | isEmptyOmittedEntries omitted = Nothing
  | otherwise = Just omitted

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

isHiddenDirectoryPath :: FilePath -> Bool
isHiddenDirectoryPath path =
  any isHiddenSegment (splitDirectories (normalizeRelativePath path))
  where
    isHiddenSegment segment =
      segment /= "."
        && segment /= ".."
        && not (null segment)
        && head segment == '.'

data GitignoredDirectoryMatcher
  = GitignoredDirectoryName FilePath
  | GitignoredDirectoryPrefix FilePath
  deriving stock (Eq, Show)

loadGitignoredDirectoryMatchers :: FilePath -> IO [GitignoredDirectoryMatcher]
loadGitignoredDirectoryMatchers projectRootPath = do
  let gitignorePath = projectRootPath </> ".gitignore"
  exists <- doesFileExist gitignorePath
  if not exists
    then pure []
    else parseGitignoredDirectoryMatchers <$> readFile gitignorePath

parseGitignoredDirectoryMatchers :: String -> [GitignoredDirectoryMatcher]
parseGitignoredDirectoryMatchers content =
  mapMaybe parseGitignoredDirectoryMatcher (lines content)

parseGitignoredDirectoryMatcher :: String -> Maybe GitignoredDirectoryMatcher
parseGitignoredDirectoryMatcher rawLine = do
  let trimmedLine = trimLine rawLine
  normalizedPattern <- normalizeGitignoredDirectoryPattern trimmedLine
  pure $
    if '/' `elem` normalizedPattern
      then GitignoredDirectoryPrefix (normalizeRelativePath normalizedPattern)
      else GitignoredDirectoryName normalizedPattern

normalizeGitignoredDirectoryPattern :: String -> Maybe FilePath
normalizeGitignoredDirectoryPattern patternLine
  | null patternLine = Nothing
  | "#" `isPrefixOf` patternLine = Nothing
  | "!" `isPrefixOf` patternLine = Nothing
  | any (`elem` patternLine) ['*', '?', '['] = Nothing
  | otherwise =
      case dropTrailingSlash (dropLeadingSlash patternLine) of
        "" -> Nothing
        normalized -> Just normalized
  where
    dropLeadingSlash path =
      case path of
        '/' : rest -> rest
        _ -> path

    dropTrailingSlash path =
      reverse (dropWhile (== '/') (reverse path))

trimLine :: String -> String
trimLine =
  dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd predicate =
      reverse . dropWhile predicate . reverse

matchesGitignoredDirectory :: [GitignoredDirectoryMatcher] -> FilePath -> Bool
matchesGitignoredDirectory matchers path =
  any (`matches` normalizedPath) matchers
  where
    normalizedPath = normalizeRelativePath path
    segments = splitDirectories normalizedPath

    matches matcher candidatePath =
      case matcher of
        GitignoredDirectoryName directoryName ->
          directoryName `elem` segments
        GitignoredDirectoryPrefix prefix ->
          isAncestorOrSelf prefix candidatePath

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
