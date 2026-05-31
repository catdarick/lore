module Lore.Internal.Package.Discovery
  ( discoverCabalManifestPaths,
    discoverManifestPaths,
    discoverStackManifestPaths,
    extractCabalProjectPackageEntries,
  )
where

import qualified Data.ByteString as BS
import Data.List (isPrefixOf, nub, sort, stripPrefix)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Lore.Internal.File (findFilesByExtensionRecursively)
import Lore.Internal.Package.Path (normalizeRelativePath)
import Lore.Internal.ProjectProvider (ProjectProvider (..))
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

discoverManifestPaths :: ProjectProvider -> FilePath -> IO (Either String [FilePath])
discoverManifestPaths provider projectRoot =
  case provider of
    StackProject ->
      discoverStackManifestPaths projectRoot
    CabalProject ->
      discoverCabalManifestPaths projectRoot

discoverStackManifestPaths :: FilePath -> IO (Either String [FilePath])
discoverStackManifestPaths projectRoot = do
  stackYamlContent <- BS.readFile (projectRoot </> "stack.yaml")
  case Yaml.decodeEither' stackYamlContent of
    Left parseError ->
      pure (Left ("Failed to parse stack.yaml: " <> show parseError))
    Right StackConfig {packages = packageEntries} -> do
      let configuredPackageEntries = map packagePath (filter (not . extraDep) packageEntries)
          localPackageEntries =
            if null configuredPackageEntries
              then ["."]
              else configuredPackageEntries
      eiManifestPaths <- mapM (resolveStackManifestPathsFromEntry projectRoot) localPackageEntries
      pure $ do
        manifestPaths <- sequence eiManifestPaths
        pure (sort (nub (concat manifestPaths)))

resolveStackManifestPathsFromEntry :: FilePath -> FilePath -> IO (Either String [FilePath])
resolveStackManifestPathsFromEntry projectRoot packageEntry
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
      isManifestFile <- doesFileExist resolvedEntryPath
      if isManifestFile
        then
          if isSupportedStackManifestPath resolvedEntryPath
            then pure (Right [resolvedEntryPath])
            else pure (Left ("stack.yaml package entry does not point to package.yaml or .cabal manifest: " <> packageEntry))
        else do
          isEntryDirectory <- doesDirectoryExist resolvedEntryPath
          if isEntryDirectory
            then resolveStackDirectoryManifestPath packageEntry resolvedEntryPath
            else pure (Left ("stack.yaml package entry does not exist: " <> packageEntry))

isSupportedStackManifestPath :: FilePath -> Bool
isSupportedStackManifestPath path =
  takeFileName path == "package.yaml" || takeExtension path == ".cabal"

resolveStackDirectoryManifestPath :: FilePath -> FilePath -> IO (Either String [FilePath])
resolveStackDirectoryManifestPath packageEntry resolvedEntryPath = do
  let packageYamlPath = resolvedEntryPath </> "package.yaml"
  packageYamlExists <- doesFileExist packageYamlPath
  entries <- listDirectory resolvedEntryPath
  let topLevelCabalFiles =
        sort
          [ resolvedEntryPath </> entry
          | entry <- entries,
            takeExtension entry == ".cabal"
          ]
  if packageYamlExists
    then pure (Right [packageYamlPath])
    else case topLevelCabalFiles of
      [singleCabalFile] -> pure (Right [singleCabalFile])
      [] -> pure (Left ("No package.yaml or .cabal file found in stack package directory: " <> packageEntry))
      _ ->
        pure
          ( Left
              ( "Multiple .cabal files found in stack package directory: "
                  <> packageEntry
                  <> ". Use an explicit package manifest path in stack.yaml."
              )
          )

discoverCabalManifestPaths :: FilePath -> IO (Either String [FilePath])
discoverCabalManifestPaths projectRoot = do
  cabalProjectExists <- doesFileExist (projectRoot </> "cabal.project")
  if cabalProjectExists
    then resolveCabalManifestPathsFromProjectFile projectRoot
    else discoverRootCabalManifestPaths projectRoot

resolveCabalManifestPathsFromProjectFile :: FilePath -> IO (Either String [FilePath])
resolveCabalManifestPathsFromProjectFile projectRoot = do
  cabalProjectText <- readFile (projectRoot </> "cabal.project")
  let packageEntries = extractCabalProjectPackageEntries cabalProjectText
  if null packageEntries
    then pure (Left "Detected Cabal project, but 'cabal.project' does not define any package entries in the packages: section.")
    else do
      eiManifestPaths <- mapM (resolveCabalManifestPathsFromEntry projectRoot) packageEntries
      pure $ do
        manifestPaths <- sequence eiManifestPaths
        pure (sort (nub (concat manifestPaths)))

discoverRootCabalManifestPaths :: FilePath -> IO (Either String [FilePath])
discoverRootCabalManifestPaths projectRoot = do
  rootEntries <- listDirectory projectRoot
  let rootManifests =
        sort
          [ projectRoot </> entry
          | entry <- rootEntries,
            takeExtension entry == ".cabal"
          ]
  case rootManifests of
    [] ->
      pure
        ( Left
            "Detected Cabal project, but no *.cabal file was found at the project root. Add a root package file or a cabal.project packages: stanza."
        )
    _ -> pure (Right rootManifests)

resolveCabalManifestPathsFromEntry :: FilePath -> FilePath -> IO (Either String [FilePath])
resolveCabalManifestPathsFromEntry projectRoot packageEntry
  | hasUnsupportedWildcard packageEntry =
      pure
        ( Left
            ( "Unsupported wildcard package entry in cabal.project: "
                <> packageEntry
                <> ". Supported wildcard tokens are '*' and '?'."
            )
        )
  | containsWildcard packageEntry =
      resolveCabalManifestPathsFromWildcardEntry projectRoot packageEntry
  | otherwise = do
      let resolvedEntryPath = normalizeRelativePath (projectRoot </> packageEntry)
      isManifestFile <- doesFileExist resolvedEntryPath
      if isManifestFile
        then
          if takeExtension resolvedEntryPath == ".cabal"
            then pure (Right [resolvedEntryPath])
            else pure (Left ("cabal.project package entry does not point to a .cabal file: " <> packageEntry))
        else do
          isEntryDirectory <- doesDirectoryExist resolvedEntryPath
          if isEntryDirectory
            then do
              cabalFiles <- sort <$> findFilesByExtensionRecursively Nothing resolvedEntryPath ".cabal"
              let topLevelCabalFiles =
                    filter ((== resolvedEntryPath) . takeDirectory) cabalFiles
              case topLevelCabalFiles of
                [singleManifest] -> pure (Right [singleManifest])
                [] -> pure (Left ("No .cabal file found in package directory: " <> packageEntry))
                _ -> pure (Left ("Multiple .cabal files found in package directory: " <> packageEntry <> ". Use explicit manifest file paths in cabal.project."))
            else pure (Left ("cabal.project package entry does not exist: " <> packageEntry))

containsWildcard :: FilePath -> Bool
containsWildcard path =
  any (`elem` ['*', '?', '[', ']']) path

hasUnsupportedWildcard :: FilePath -> Bool
hasUnsupportedWildcard path =
  any (`elem` ['[', ']']) path

resolveCabalManifestPathsFromWildcardEntry :: FilePath -> FilePath -> IO (Either String [FilePath])
resolveCabalManifestPathsFromWildcardEntry projectRoot packageEntry = do
  candidateCabalFiles <- sort <$> findFilesByExtensionRecursively Nothing projectRoot ".cabal"
  let normalizedPatternSegments = splitDirectories (normalizeRelativePath packageEntry)
      matches =
        [ path
        | path <- candidateCabalFiles,
          let relativePath = normalizeRelativePath (makeRelative projectRoot path),
          wildcardPathMatches normalizedPatternSegments (splitDirectories relativePath)
        ]
  if null matches
    then pure (Left ("No package manifests matched cabal.project wildcard entry: " <> packageEntry))
    else pure (Right matches)

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
