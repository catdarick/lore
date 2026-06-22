module Lore.Internal.Ghc.PackageEnvironment.Probe
  ( GhcEnvironmentProbeRunner (..),
    captureGhcEnvironment,
    captureGhcEnvironmentWithRunner,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Distribution.Version as CabalVersion
import Lore.Internal.Ghc.PackageEnvironment.Index (buildPackageIndex)
import Lore.Internal.Ghc.PackageEnvironment.Parse
  ( defaultPackageDbStack,
    packagePathToPackageDbStack,
    parseGhcEnvironmentFile,
  )
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( CapturedGhcEnvironment (..),
    GhcToolchain (..),
    PackageDbStack,
    PackageEnvironmentSnapshot (..),
    PackageIndex (..),
    PackageIndexEntry (..),
    PackageNameText (..),
    ParsedGhcEnvironmentFile (..),
    UnitIdText (..),
  )
import Lore.Internal.ProjectProvider (ProjectProvider)
import Lore.Internal.ProjectProvider.Ops (providerRunInEnvironment)
import System.Directory (doesFileExist)
import System.FilePath (isRelative, normalise, (</>))
import Text.Read (readMaybe)

data GhcEnvironmentProbeRunner = GhcEnvironmentProbeRunner
  { runBuildToolProbe :: ProjectProvider -> FilePath -> String -> IO (Either String String),
    runBuildPackageIndex :: FilePath -> FilePath -> PackageDbStack -> IO (Either String PackageIndex)
  }

defaultGhcEnvironmentProbeRunner :: GhcEnvironmentProbeRunner
defaultGhcEnvironmentProbeRunner =
  GhcEnvironmentProbeRunner
    { runBuildToolProbe = providerRunInEnvironment,
      runBuildPackageIndex = buildPackageIndex
    }

captureGhcEnvironment :: ProjectProvider -> FilePath -> IO (Either String CapturedGhcEnvironment)
captureGhcEnvironment =
  captureGhcEnvironmentWithRunner defaultGhcEnvironmentProbeRunner

captureGhcEnvironmentWithRunner :: GhcEnvironmentProbeRunner -> ProjectProvider -> FilePath -> IO (Either String CapturedGhcEnvironment)
captureGhcEnvironmentWithRunner runner provider projectRoot = do
  executionResult <- runner.runBuildToolProbe provider projectRoot renderEnvironmentProbeScript
  case executionResult of
    Left err ->
      pure
        ( Left
            ( "Failed to capture GHC environment via build tool execution context. "
                <> ensureTrailingPeriod err
            )
        )
    Right output ->
      case parseRawGhcEnvironment output of
        Left err -> pure (Left err)
        Right rawGhcEnvironment -> do
          normalizedResult <- normalizeRawGhcEnvironment projectRoot rawGhcEnvironment
          case normalizedResult of
            Left err -> pure (Left err)
            Right normalizedEnvironment -> do
              packageIndexResult <-
                runner.runBuildPackageIndex
                  projectRoot
                  normalizedEnvironment.normalizedGhcPkgExe
                  normalizedEnvironment.normalizedPackageDbStack
              pure do
                packageIndex <- packageIndexResult
                selectedUnitIdsByPackageName <-
                  validateSelectedUnitIds
                    normalizedEnvironment.normalizedSelectedUnitIds
                    packageIndex
                pure
                  CapturedGhcEnvironment
                    { capturedGhcToolchain =
                        GhcToolchain
                          { ghcToolchainCompilerExe = normalizedEnvironment.normalizedGhcExe,
                            ghcToolchainCompilerVersion = normalizedEnvironment.normalizedGhcVersion,
                            ghcToolchainGhcPkgExe = normalizedEnvironment.normalizedGhcPkgExe,
                            ghcToolchainLibDir = normalizedEnvironment.normalizedGhcLibDir
                          },
                      capturedPackageEnvironment =
                        PackageEnvironmentSnapshot
                          { packageEnvironmentPackageDbStack = normalizedEnvironment.normalizedPackageDbStack,
                            packageEnvironmentPackageIndex = packageIndex,
                            packageEnvironmentSelectedUnitIdsByPackageName = selectedUnitIdsByPackageName
                          }
                    }

data RawGhcEnvironment = RawGhcEnvironment
  { rawGhcExe :: FilePath,
    rawGhcVersion :: String,
    rawGhcPkgExe :: FilePath,
    rawGhcLibDir :: FilePath,
    rawGhcEnvironmentValue :: Maybe FilePath,
    rawGhcEnvironmentContents :: Maybe T.Text,
    rawGhcPackagePathValue :: Maybe String
  }

data RawPackageDbSource
  = RawPackageEnvironmentFile FilePath T.Text
  | RawPackagePath String
  | RawDefaultPackageDbs

data NormalizedGhcEnvironment = NormalizedGhcEnvironment
  { normalizedGhcExe :: FilePath,
    normalizedGhcVersion :: CabalVersion.Version,
    normalizedGhcPkgExe :: FilePath,
    normalizedGhcLibDir :: FilePath,
    normalizedPackageDbStack :: PackageDbStack,
    normalizedSelectedUnitIds :: Set.Set UnitIdText
  }

normalizeRawGhcEnvironment :: FilePath -> RawGhcEnvironment -> IO (Either String NormalizedGhcEnvironment)
normalizeRawGhcEnvironment projectRoot rawGhcEnvironment = do
  packageDbSourceResult <- resolveRawPackageDbSource projectRoot rawGhcEnvironment
  pure do
    ghcVersion <- parseVersionText rawGhcEnvironment.rawGhcVersion
    packageDbSource <- packageDbSourceResult
    normalizedPackageEnvironment <- parseRawPackageDbSource packageDbSource
    pure
      NormalizedGhcEnvironment
        { normalizedGhcExe = rawGhcEnvironment.rawGhcExe,
          normalizedGhcVersion = ghcVersion,
          normalizedGhcPkgExe = rawGhcEnvironment.rawGhcPkgExe,
          normalizedGhcLibDir = rawGhcEnvironment.rawGhcLibDir,
          normalizedPackageDbStack = normalizedPackageEnvironment.normalizedPackageDbStack,
          normalizedSelectedUnitIds = normalizedPackageEnvironment.normalizedSelectedUnitIds
        }

data NormalizedPackageEnvironment = NormalizedPackageEnvironment
  { normalizedPackageDbStack :: PackageDbStack,
    normalizedSelectedUnitIds :: Set.Set UnitIdText
  }

parseRawPackageDbSource :: RawPackageDbSource -> Either String NormalizedPackageEnvironment
parseRawPackageDbSource packageDbSource =
  case packageDbSource of
    RawPackageEnvironmentFile environmentPath environmentContents -> do
      parsedEnvironmentFile <-
        parseGhcEnvironmentFile environmentPath environmentContents
      pure
        NormalizedPackageEnvironment
          { normalizedPackageDbStack = parsedEnvironmentFile.parsedEnvPackageDbStack,
            normalizedSelectedUnitIds = parsedEnvironmentFile.parsedEnvSelectedUnitIds
          }
    RawPackagePath packagePathValue ->
      pure
        NormalizedPackageEnvironment
          { normalizedPackageDbStack = packagePathToPackageDbStack packagePathValue,
            normalizedSelectedUnitIds = Set.empty
          }
    RawDefaultPackageDbs ->
      pure
        NormalizedPackageEnvironment
          { normalizedPackageDbStack = defaultPackageDbStack,
            normalizedSelectedUnitIds = Set.empty
          }

resolveRawPackageDbSource :: FilePath -> RawGhcEnvironment -> IO (Either String RawPackageDbSource)
resolveRawPackageDbSource projectRoot rawGhcEnvironment =
  case rawGhcEnvironment.rawGhcEnvironmentValue of
    Just "-" ->
      case rawGhcEnvironment.rawGhcPackagePathValue of
        Just packagePathValue ->
          pure (Right (RawPackagePath packagePathValue))
        Nothing ->
          pure (Right RawDefaultPackageDbs)
    Just environmentPath -> do
      case rawGhcEnvironment.rawGhcEnvironmentContents of
        Just environmentContents -> do
          let normalizedEnvironmentPath = normalizeEnvironmentPath projectRoot environmentPath
          pure (Right (RawPackageEnvironmentFile normalizedEnvironmentPath environmentContents))
        Nothing -> do
          let normalizedEnvironmentPath = normalizeEnvironmentPath projectRoot environmentPath
          environmentFileExists <- doesFileExist normalizedEnvironmentPath
          if environmentFileExists
            then do
              environmentContents <- readFile normalizedEnvironmentPath
              pure (Right (RawPackageEnvironmentFile normalizedEnvironmentPath (T.pack environmentContents)))
            else
              pure
                ( Left
                    ( "GHC_ENVIRONMENT was set, but the referenced file does not exist: "
                        <> normalizedEnvironmentPath
                        <> "."
                    )
                )
    Nothing ->
      case rawGhcEnvironment.rawGhcPackagePathValue of
        Just packagePathValue ->
          pure (Right (RawPackagePath packagePathValue))
        Nothing ->
          pure (Right RawDefaultPackageDbs)

normalizeEnvironmentPath :: FilePath -> FilePath -> FilePath
normalizeEnvironmentPath projectRoot environmentPath
  | isRelative environmentPath = normalise (projectRoot </> environmentPath)
  | otherwise = normalise environmentPath

parseRawGhcEnvironment :: String -> Either String RawGhcEnvironment
parseRawGhcEnvironment output = do
  let outputLines = lines output
  ghcExe <- readRequiredProbeField output ghcExecutablePrefix
  ghcVersion <- readRequiredProbeField output ghcVersionPrefix
  ghcPkgExe <- readRequiredProbeField output ghcPkgExecutablePrefix
  ghcLibDir <- readRequiredProbeField output ghcLibDirPrefix
  ghcEnvironmentRaw <- readRequiredProbeField output ghcEnvironmentPrefix
  ghcPackagePathRaw <- readRequiredProbeField output ghcPackagePathPrefix
  ghcEnvironmentContents <- extractEnvironmentContents outputLines
  pure
    RawGhcEnvironment
      { rawGhcExe = ghcExe,
        rawGhcVersion = ghcVersion,
        rawGhcPkgExe = ghcPkgExe,
        rawGhcLibDir = ghcLibDir,
        rawGhcEnvironmentValue = nonEmptyValue ghcEnvironmentRaw,
        rawGhcEnvironmentContents = ghcEnvironmentContents,
        rawGhcPackagePathValue = nonEmptyValue ghcPackagePathRaw
      }

nonEmptyValue :: String -> Maybe String
nonEmptyValue value =
  if null value
    then Nothing
    else Just value

readRequiredProbeField :: String -> String -> Either String String
readRequiredProbeField output prefix =
  case List.find (List.isPrefixOf prefix) (lines output) of
    Nothing ->
      Left
        ( "Missing expected environment probe marker '"
            <> prefix
            <> "' in build-tool output."
        )
    Just markerLine ->
      Right (drop (length prefix) markerLine)

extractEnvironmentContents :: [String] -> Either String (Maybe T.Text)
extractEnvironmentContents outputLines =
  case dropWhile (/= ghcEnvironmentContentsBeginMarker) outputLines of
    [] -> Right Nothing
    _beginMarker : remainingLines ->
      let (contentLines, trailingLines) = break (== ghcEnvironmentContentsEndMarker) remainingLines
       in case trailingLines of
            [] ->
              Left
                ( "Missing closing marker '"
                    <> ghcEnvironmentContentsEndMarker
                    <> "' while parsing GHC environment contents."
                )
            _endMarker : _ ->
              Right (Just (T.pack (unlines contentLines)))

renderEnvironmentProbeScript :: String
renderEnvironmentProbeScript =
  unlines
    [ "set -eu",
      "compiler=\"$(command -v ghc)\"",
      "compiler_dir=\"$(dirname \"$compiler\")\"",
      "compiler_version=\"$($compiler --numeric-version)\"",
      "if [ -x \"$compiler_dir/ghc-pkg-$compiler_version\" ]; then",
      "  ghc_pkg=\"$compiler_dir/ghc-pkg-$compiler_version\"",
      "elif [ -x \"$compiler_dir/ghc-pkg\" ]; then",
      "  ghc_pkg=\"$compiler_dir/ghc-pkg\"",
      "else",
      "  ghc_pkg=\"$(command -v ghc-pkg)\"",
      "fi",
      "libdir=\"$($compiler --print-libdir)\"",
      "printf \"__LORE_GHC_EXE__:%s\\n\" \"$compiler\"",
      "printf \"__LORE_GHC_VERSION__:%s\\n\" \"$compiler_version\"",
      "printf \"__LORE_GHC_PKG_EXE__:%s\\n\" \"$ghc_pkg\"",
      "printf \"__LORE_GHC_LIBDIR__:%s\\n\" \"$libdir\"",
      "printf \"__LORE_GHC_ENVIRONMENT__:%s\\n\" \"${GHC_ENVIRONMENT:-}\"",
      "printf \"__LORE_GHC_PACKAGE_PATH__:%s\\n\" \"${GHC_PACKAGE_PATH:-}\"",
      "if [ -n \"${GHC_ENVIRONMENT:-}\" ] && [ \"${GHC_ENVIRONMENT}\" != \"-\" ] && [ -f \"$GHC_ENVIRONMENT\" ]; then",
      "  printf \"__LORE_GHC_ENVIRONMENT_CONTENT_BEGIN__\\n\"",
      "  cat \"$GHC_ENVIRONMENT\"",
      "  printf \"__LORE_GHC_ENVIRONMENT_CONTENT_END__\\n\"",
      "fi"
    ]

ghcExecutablePrefix :: String
ghcExecutablePrefix = "__LORE_GHC_EXE__:"

ghcVersionPrefix :: String
ghcVersionPrefix = "__LORE_GHC_VERSION__:"

ghcPkgExecutablePrefix :: String
ghcPkgExecutablePrefix = "__LORE_GHC_PKG_EXE__:"

ghcLibDirPrefix :: String
ghcLibDirPrefix = "__LORE_GHC_LIBDIR__:"

ghcEnvironmentPrefix :: String
ghcEnvironmentPrefix = "__LORE_GHC_ENVIRONMENT__:"

ghcPackagePathPrefix :: String
ghcPackagePathPrefix = "__LORE_GHC_PACKAGE_PATH__:"

ghcEnvironmentContentsBeginMarker :: String
ghcEnvironmentContentsBeginMarker = "__LORE_GHC_ENVIRONMENT_CONTENT_BEGIN__"

ghcEnvironmentContentsEndMarker :: String
ghcEnvironmentContentsEndMarker = "__LORE_GHC_ENVIRONMENT_CONTENT_END__"

ensureTrailingPeriod :: String -> String
ensureTrailingPeriod text
  | null text = text
  | last text == '.' = text
  | otherwise = text <> "."

parseVersionText :: String -> Either String CabalVersion.Version
parseVersionText raw =
  case traverse readMaybe (splitOnChar '.' raw) of
    Just parts
      | not (null parts) ->
          Right (CabalVersion.mkVersion parts)
    _ ->
      Left ("Failed to parse GHC version from environment probe: " <> raw)

splitOnChar :: Char -> String -> [String]
splitOnChar delimiter = go []
  where
    go currentChunk remaining =
      case remaining of
        [] ->
          [reverse currentChunk]
        nextChar : restChars
          | nextChar == delimiter ->
              reverse currentChunk : go [] restChars
          | otherwise ->
              go (nextChar : currentChunk) restChars

validateSelectedUnitIds ::
  Set.Set UnitIdText ->
  PackageIndex ->
  Either String (Map.Map PackageNameText (Set.Set UnitIdText))
validateSelectedUnitIds selectedUnitIds packageIndex = do
  selectedEntries <- mapM lookupSelectedEntry (Set.toAscList selectedUnitIds)
  pure
    ( Map.fromListWith
        (<>)
        [ (entry.packageIndexPackageName, Set.singleton entry.packageIndexUnitId)
        | entry <- selectedEntries
        ]
    )
  where
    lookupSelectedEntry selectedUnitId =
      case Map.lookup selectedUnitId packageIndex.packageIndexByUnitId of
        Nothing ->
          Left
            ( "GHC environment selected unit-id '"
                <> selectedUnitId.unUnitIdText
                <> "', but it was not present in the ghc-pkg package index dump."
            )
        Just packageIndexEntry ->
          Right packageIndexEntry
