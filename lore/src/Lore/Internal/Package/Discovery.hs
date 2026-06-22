module Lore.Internal.Package.Discovery
  ( discoverStackPackageRoots,
    discoverCabalPackageRoots,
    extractCabalProjectPackageEntries,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, sort, stripPrefix)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Lore.Internal.File (findFilesByExtensionRecursively)
import Lore.Internal.Package.Path (normalizeRelativePath)
import Lore.Internal.Package.Root (PackageRoot (..), normalizePackageRoots)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, splitDirectories, takeDirectory, takeExtension, takeFileName, (</>))

data StackConfig = StackConfig
  { packages :: [StackPackageEntry]
  }

data StackPackageEntry = StackPackageEntry
  { packagePath :: FilePath,
    extraDep :: Bool
  }

instance Yaml.FromJSON StackConfig where
  parseJSON = Yaml.withObject "StackConfig" \obj ->
    StackConfig
      <$> obj Yaml..:? "packages" Yaml..!= []

instance Yaml.FromJSON StackPackageEntry where
  parseJSON = \case
    Yaml.String pathText ->
      pure
        StackPackageEntry
          { packagePath = T.unpack pathText,
            extraDep = False
          }
    value ->
      Yaml.withObject "StackPackageEntry" parsePackageObject value
    where
      parsePackageObject obj = do
        locationText <- obj Yaml..: "location"
        isExtraDep <- obj Yaml..:? "extra-dep" Yaml..!= False
        pure
          StackPackageEntry
            { packagePath = T.unpack locationText,
              extraDep = isExtraDep
            }

discoverStackPackageRoots :: FilePath -> IO (Either String [PackageRoot])
discoverStackPackageRoots projectRoot = do
  let stackYamlPath = projectRoot </> "stack.yaml"
  eiStackYamlContent <- try (BS.readFile stackYamlPath) :: IO (Either IOException BS.ByteString)
  case eiStackYamlContent of
    Left ioErr -> pure (Left ("Failed to read stack project file " <> stackYamlPath <> ": " <> show ioErr))
    Right stackYamlContent ->
      case Yaml.decodeEither' stackYamlContent of
        Left parseError ->
          pure (Left ("Failed to parse stack.yaml: " <> show parseError))
        Right StackConfig {packages = packageEntries} -> do
          let configuredPackageEntries = map packagePath (filter (not . extraDep) packageEntries)
              localPackageEntries =
                if null configuredPackageEntries
                  then ["."]
                  else configuredPackageEntries
          eiRoots <- mapM (resolveStackPackageRootFromEntry projectRoot) localPackageEntries
          pure (normalizePackageRoots <$> sequence eiRoots)

resolveStackPackageRootFromEntry :: FilePath -> FilePath -> IO (Either String PackageRoot)
resolveStackPackageRootFromEntry projectRoot packageEntry
  | containsWildcard packageEntry =
      pure
        ( Left
            ( "Unsupported wildcard package entry in stack.yaml: "
                <> packageEntry
                <> ". Please list package directories explicitly in the packages: section."
            )
        )
  | otherwise = do
      let resolvedEntryPath = normalizeRelativePath (projectRoot </> packageEntry)
      isFile <- doesFileExist resolvedEntryPath
      if isFile
        then
          if takeFileName resolvedEntryPath == "package.yaml"
            then
              pure
                ( Right
                    PackageRoot
                      { packageRootPath = takeDirectory resolvedEntryPath,
                        packageRootPreferredCabalFile = Nothing
                      }
                )
            else
              if takeExtension resolvedEntryPath == ".cabal"
                then
                  pure
                    ( Right
                        PackageRoot
                          { packageRootPath = takeDirectory resolvedEntryPath,
                            packageRootPreferredCabalFile = Just resolvedEntryPath
                          }
                    )
                else pure (Left ("stack.yaml package entry must point to a package directory, package.yaml, or .cabal file: " <> packageEntry))
        else do
          isDir <- doesDirectoryExist resolvedEntryPath
          if isDir
            then
              pure
                ( Right
                    PackageRoot
                      { packageRootPath = resolvedEntryPath,
                        packageRootPreferredCabalFile = Nothing
                      }
                )
            else pure (Left ("stack.yaml package entry does not exist: " <> packageEntry))

discoverCabalPackageRoots :: FilePath -> IO (Either String [PackageRoot])
discoverCabalPackageRoots projectRoot = do
  cabalProjectExists <- doesFileExist (projectRoot </> "cabal.project")
  if cabalProjectExists
    then resolveCabalPackageRootsFromProjectFile projectRoot
    else
      pure
        ( Right
            [ PackageRoot
                { packageRootPath = normalizeRelativePath projectRoot,
                  packageRootPreferredCabalFile = Nothing
                }
            ]
        )

resolveCabalPackageRootsFromProjectFile :: FilePath -> IO (Either String [PackageRoot])
resolveCabalPackageRootsFromProjectFile projectRoot = do
  let cabalProjectPath = projectRoot </> "cabal.project"
  eiCabalProjectText <- try (readFile cabalProjectPath) :: IO (Either IOException String)
  case eiCabalProjectText of
    Left ioErr -> pure (Left ("Failed to read Cabal project file " <> cabalProjectPath <> ": " <> show ioErr))
    Right cabalProjectText -> do
      let packageEntries = extractCabalProjectPackageEntries cabalProjectText
      if null packageEntries
        then pure (Left "Detected Cabal project, but 'cabal.project' does not define any package entries in the packages: section.")
        else do
          eiRoots <- mapM (resolveCabalPackageRootsFromEntry projectRoot) packageEntries
          pure $ do
            roots <- sequence eiRoots
            pure (normalizePackageRoots (concat roots))

resolveCabalPackageRootsFromEntry :: FilePath -> FilePath -> IO (Either String [PackageRoot])
resolveCabalPackageRootsFromEntry projectRoot packageEntry
  | hasUnsupportedWildcard packageEntry =
      pure
        ( Left
            ( "Unsupported wildcard package entry in cabal.project: "
                <> packageEntry
                <> ". Supported wildcard tokens are '*' and '?'."
            )
        )
  | containsWildcard packageEntry =
      resolveCabalPackageRootsFromWildcardEntry projectRoot packageEntry
  | otherwise = do
      let resolvedEntryPath = normalizeRelativePath (projectRoot </> packageEntry)
      isFile <- doesFileExist resolvedEntryPath
      if isFile
        then
          if takeFileName resolvedEntryPath == "package.yaml"
            then
              pure
                ( Right
                    [ PackageRoot
                        { packageRootPath = takeDirectory resolvedEntryPath,
                          packageRootPreferredCabalFile = Nothing
                        }
                    ]
                )
            else
              if takeExtension resolvedEntryPath == ".cabal"
                then
                  pure
                    ( Right
                        [ PackageRoot
                            { packageRootPath = takeDirectory resolvedEntryPath,
                              packageRootPreferredCabalFile = Just resolvedEntryPath
                            }
                        ]
                    )
                else pure (Left ("cabal.project package entry must point to a package directory, package.yaml, or .cabal file: " <> packageEntry))
        else do
          isDir <- doesDirectoryExist resolvedEntryPath
          if isDir
            then
              pure
                ( Right
                    [ PackageRoot
                        { packageRootPath = resolvedEntryPath,
                          packageRootPreferredCabalFile = Nothing
                        }
                    ]
                )
            else pure (Left ("cabal.project package entry does not exist: " <> packageEntry))

resolveCabalPackageRootsFromWildcardEntry :: FilePath -> FilePath -> IO (Either String [PackageRoot])
resolveCabalPackageRootsFromWildcardEntry projectRoot packageEntry
  | takeExtension packageEntry == ".cabal" = do
      candidateCabalFiles <- sort <$> findFilesByExtensionRecursively Nothing projectRoot ".cabal"
      let normalizedPatternSegments = splitDirectories (normalizeRelativePath packageEntry)
          matches =
            [ PackageRoot
                { packageRootPath = takeDirectory path,
                  packageRootPreferredCabalFile = Just path
                }
            | path <- candidateCabalFiles,
              let relativePath = normalizeRelativePath (makeRelative projectRoot path),
              wildcardPathMatches normalizedPatternSegments (splitDirectories relativePath)
            ]
      if null matches
        then pure (Left ("No package manifests matched cabal.project wildcard entry: " <> packageEntry))
        else pure (Right (normalizePackageRoots matches))
  | otherwise = do
      candidateDirectories <- listDirectoriesRecursively defaultPackageDiscoveryIgnoredDirs projectRoot
      let normalizedPatternSegments = splitDirectories (normalizeRelativePath packageEntry)
      wildcardMatches <-
        filterM
          (directoryLooksLikePackageRoot . (projectRoot </>))
          [ relativeDir
          | relativeDir <- map (normalizeRelativePath . makeRelative projectRoot) candidateDirectories,
            wildcardPathMatches normalizedPatternSegments (splitDirectories relativeDir)
          ]
      let matchedRoots =
            [ PackageRoot
                { packageRootPath = normalizeRelativePath (projectRoot </> relativeDir),
                  packageRootPreferredCabalFile = Nothing
                }
            | relativeDir <- wildcardMatches
            ]
      if null matchedRoots
        then pure (Left ("No package roots matched cabal.project wildcard entry: " <> packageEntry))
        else pure (Right (normalizePackageRoots matchedRoots))

containsWildcard :: FilePath -> Bool
containsWildcard path =
  any (`elem` ['*', '?', '[', ']']) path

hasUnsupportedWildcard :: FilePath -> Bool
hasUnsupportedWildcard path =
  any (`elem` ['[', ']']) path

defaultPackageDiscoveryIgnoredDirs :: Set.Set FilePath
defaultPackageDiscoveryIgnoredDirs =
  Set.fromList
    [ ".git",
      ".stack-work",
      "dist",
      "dist-newstyle",
      ".direnv",
      ".cabal-sandbox"
    ]

listDirectoriesRecursively :: Set.Set FilePath -> FilePath -> IO [FilePath]
listDirectoriesRecursively ignoredDirNames rootDir = do
  directEntries <- listDirectory rootDir
  directSubdirs <-
    filterM
      doesDirectoryExist
      [rootDir </> entry | entry <- directEntries, not (Set.member entry ignoredDirNames)]
  nestedSubdirs <- concat <$> mapM (listDirectoriesRecursively ignoredDirNames) directSubdirs
  pure (rootDir : nestedSubdirs)

directoryLooksLikePackageRoot :: FilePath -> IO Bool
directoryLooksLikePackageRoot dir = do
  packageYamlExists <- doesFileExist (dir </> "package.yaml")
  cabalFiles <- listTopLevelCabalFiles dir
  pure (packageYamlExists || not (null cabalFiles))

listTopLevelCabalFiles :: FilePath -> IO [FilePath]
listTopLevelCabalFiles dir = do
  entries <- listDirectory dir
  pure
    ( sort
        [ dir </> entry
        | entry <- entries,
          takeExtension entry == ".cabal"
        ]
    )

wildcardPathMatches :: [FilePath] -> [FilePath] -> Bool
wildcardPathMatches patternSegments candidateSegments =
  length patternSegments == length candidateSegments
    && and (zipWith wildcardSegmentMatches patternSegments candidateSegments)

wildcardSegmentMatches :: String -> String -> Bool
wildcardSegmentMatches patternText candidateText =
  go patternText candidateText
  where
    go [] [] = True
    go [] _ = False
    go ('*' : patternTail) text =
      go patternTail text
        || case text of
          [] -> False
          (_ : textTail) -> go ('*' : patternTail) textTail
    go ('?' : patternTail) (_ : textTail) =
      go patternTail textTail
    go ('?' : _) [] = False
    go (patternChar : patternTail) (textChar : textTail)
      | patternChar == textChar = go patternTail textTail
      | otherwise = False
    go _ _ = False

extractCabalProjectPackageEntries :: String -> [FilePath]
extractCabalProjectPackageEntries = go False . lines
  where
    go _ [] = []
    go inPackages (line : rest)
      | startsPackagesSection strippedLine =
          inlineEntries <> go True rest
      | inPackages && isIndentedLine line =
          parsePackageEntryList strippedLine <> go True rest
      | inPackages =
          go False (line : rest)
      | otherwise =
          go False rest
      where
        strippedLine = stripLineComments (trim line)
        inlineEntries =
          case stripPrefix "packages:" strippedLine of
            Just remainder -> parsePackageEntryList remainder
            Nothing -> []

    startsPackagesSection line =
      "packages:" `isPrefixOf` line

    isIndentedLine = \case
      [] -> False
      firstChar : _ -> firstChar == ' ' || firstChar == '\t'

    stripLineComments text =
      takeWhileBeforeComment text

    takeWhileBeforeComment [] = []
    takeWhileBeforeComment ('-' : '-' : _) = []
    takeWhileBeforeComment (char : chars) = char : takeWhileBeforeComment chars

    parsePackageEntryList = filter (not . null) . concatMap splitComma . words . trim

    splitComma text =
      map trim (splitByComma text)

    splitByComma text =
      case break (== ',') text of
        (entry, []) -> [entry]
        (entry, _ : rest) -> entry : splitByComma rest

    trim = reverse . dropWhile isSpaceChar . reverse . dropWhile isSpaceChar

    isSpaceChar ch = ch `elem` [' ', '\t']
