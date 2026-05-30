module Lore.Internal.PackageDB
  ( ProjectProvider (..),
    PackageDbDirective (..),
    PackageExposure (..),
    ResolvedPackageEnvironment (..),
    detectProjectProvider,
    resolvePackageEnvironment,
    parseGhcEnvironmentFile,
    packageEnvironmentCacheKey,
    withDependencyPackageExposures,
  )
where

import Control.Exception (IOException, handle)
import Data.Char (isSpace)
import Data.List (stripPrefix)
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (isRelative, normalise, splitSearchPath, takeDirectory, takeExtension, (</>))
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

data ProjectProvider
  = StackProject
  | CabalProject
  deriving (Eq, Ord, Show)

data PackageDbDirective
  = ClearPackageDb
  | GlobalPackageDb
  | UserPackageDb
  | SpecificPackageDb FilePath
  deriving (Eq, Ord, Show)

data PackageExposure
  = ExposePackageName String
  | ExposePackageId String
  deriving (Eq, Ord, Show)

data ResolvedPackageEnvironment = ResolvedPackageEnvironment
  { envPackageDbDirectives :: [PackageDbDirective],
    envPackageExposures :: [PackageExposure]
  }
  deriving (Eq, Show)

detectProjectProvider :: FilePath -> IO (Either String ProjectProvider)
detectProjectProvider projectRoot = do
  stackConfigExists <- doesFileExist (projectRoot </> "stack.yaml")
  if stackConfigExists
    then pure (Right StackProject)
    else do
      cabalProjectExists <- doesFileExist (projectRoot </> "cabal.project")
      if cabalProjectExists
        then pure (Right CabalProject)
        else detectProviderFromRootCabalFile projectRoot

resolvePackageEnvironment :: ProjectProvider -> FilePath -> IO (Either String ResolvedPackageEnvironment)
resolvePackageEnvironment provider projectRoot =
  case provider of
    StackProject -> resolveStackPackageEnvironment projectRoot
    CabalProject -> resolveCabalPackageEnvironment projectRoot

parseGhcEnvironmentFile :: FilePath -> T.Text -> Either String ResolvedPackageEnvironment
parseGhcEnvironmentFile environmentFilePath content =
  foldl parseLine (Right emptyEnvironment) (zip [1 :: Int ..] (T.lines content))
  where
    parseLine :: Either String ResolvedPackageEnvironment -> (Int, T.Text) -> Either String ResolvedPackageEnvironment
    parseLine acc (lineNumber, rawLine) = do
      env <- acc
      case parseDirective lineNumber rawLine of
        Nothing -> pure env
        Just (Left err) -> Left err
        Just (Right directive) -> pure (applyDirective env directive)

    parseDirective :: Int -> T.Text -> Maybe (Either String ParsedDirective)
    parseDirective lineNumber rawLine
      | T.null line = Nothing
      | "--" `T.isPrefixOf` line = Nothing
      | line == "clear-package-db" = Just (Right (DbDirective ClearPackageDb))
      | line == "global-package-db" = Just (Right (DbDirective GlobalPackageDb))
      | line == "user-package-db" = Just (Right (DbDirective UserPackageDb))
      | "package-db " `T.isPrefixOf` line =
          Just $ do
            path <- extractArgument "package-db" line
            pure $ DbDirective (SpecificPackageDb (resolvePackageDbPath path))
      | "package-id " `T.isPrefixOf` line =
          Just $ do
            packageId <- extractArgument "package-id" line
            pure $ ExposureDirective (ExposePackageId packageId)
      | otherwise =
          Just $ Left $ "Unsupported GHC environment directive at line " <> show lineNumber <> ": " <> T.unpack line
      where
        line = T.strip rawLine

        extractArgument :: String -> T.Text -> Either String FilePath
        extractArgument keyword directiveLine =
          let argument = T.unpack (T.strip (T.drop (length keyword) directiveLine))
           in if null argument
                then Left $ "Missing argument for directive '" <> keyword <> "'"
                else Right argument

    resolvePackageDbPath :: FilePath -> FilePath
    resolvePackageDbPath path
      | isRelative path = normalise (takeDirectory environmentFilePath </> path)
      | otherwise = normalise path

resolveStackPackageEnvironment :: FilePath -> IO (Either String ResolvedPackageEnvironment)
resolveStackPackageEnvironment projectRoot = do
  res <- runProcess' projectRoot "stack" ["path", "--ghc-package-path"]
  pure $ do
    output <- firstError "Failed to resolve package database paths" res
    let packageDbPaths = normalizeSearchPathOutput output
    pure
      ResolvedPackageEnvironment
        { envPackageDbDirectives = ClearPackageDb : map SpecificPackageDb packageDbPaths,
          envPackageExposures = []
        }

resolveCabalPackageEnvironment :: FilePath -> IO (Either String ResolvedPackageEnvironment)
resolveCabalPackageEnvironment projectRoot = do
  executionResult <-
    runProcess'
      projectRoot
      "cabal"
      ["exec", "--write-ghc-environment-files=never", "--", "sh", "-lc", renderEnvironmentCaptureScript]
  pure $ do
    output <-
      firstError
        "Detected Cabal project, but failed to resolve package environment via cabal exec."
        executionResult
    (environmentPath, environmentContents) <- parseCapturedEnvironmentOutput output
    firstError
      ("Failed to parse GHC environment file: " <> environmentPath)
      (parseGhcEnvironmentFile environmentPath (T.pack environmentContents))

withDependencyPackageExposures :: ProjectProvider -> Set.Set String -> ResolvedPackageEnvironment -> ResolvedPackageEnvironment
withDependencyPackageExposures provider dependencyNames environment =
  case provider of
    StackProject ->
      environment
        { envPackageExposures = dedupeExposures (environment.envPackageExposures <> map ExposePackageName (Set.toAscList dependencyNames))
        }
    CabalProject ->
      environment

data ParsedDirective
  = DbDirective PackageDbDirective
  | ExposureDirective PackageExposure

emptyEnvironment :: ResolvedPackageEnvironment
emptyEnvironment =
  ResolvedPackageEnvironment
    { envPackageDbDirectives = [],
      envPackageExposures = []
    }

applyDirective :: ResolvedPackageEnvironment -> ParsedDirective -> ResolvedPackageEnvironment
applyDirective environment directive =
  case directive of
    DbDirective dbDirective ->
      environment
        { envPackageDbDirectives = environment.envPackageDbDirectives <> [dbDirective]
        }
    ExposureDirective exposure ->
      environment
        { envPackageExposures = environment.envPackageExposures <> [exposure]
        }

detectProviderFromRootCabalFile :: FilePath -> IO (Either String ProjectProvider)
detectProviderFromRootCabalFile projectRoot = do
  entries <- listDirectory projectRoot
  let rootCabalFiles = filter ((== ".cabal") . takeExtension) entries
  case rootCabalFiles of
    [_] -> pure (Right CabalProject)
    [] ->
      pure
        ( Left
            "No supported project files were found. Expected one of: stack.yaml, cabal.project, or a single *.cabal file at the project root."
        )
    _ ->
      pure
        ( Left
            "Multiple root-level *.cabal files were found without a cabal.project file. Please add cabal.project to define package selection explicitly."
        )

parseCapturedEnvironmentOutput :: String -> Either String (FilePath, String)
parseCapturedEnvironmentOutput output =
  case dropWhile (not . isEnvironmentPathLine) (lines output) of
    pathLine : remainingLines ->
      case stripPrefix environmentPathPrefix pathLine of
        Nothing ->
          Left
            ( "Failed to capture GHC environment path from cabal exec output. Expected prefix '"
                <> environmentPathPrefix
                <> "'."
            )
        Just environmentPath
          | null environmentPath ->
              Left "cabal exec reported an empty GHC environment path."
          | otherwise ->
              Right (environmentPath, unlines remainingLines)
    [] ->
      Left "cabal exec output did not contain a GHC environment path marker."
  where
    isEnvironmentPathLine line = case stripPrefix environmentPathPrefix line of
      Just _ -> True
      Nothing -> False

environmentPathPrefix :: String
environmentPathPrefix = "__LORE_GHC_ENV_PATH__:"

packageEnvironmentCacheKey :: ResolvedPackageEnvironment -> Set.Set String
packageEnvironmentCacheKey environment =
  Set.fromList
    ( map renderDirectiveKey environment.envPackageDbDirectives
        <> map renderExposureKey environment.envPackageExposures
    )
  where
    renderDirectiveKey directive =
      case directive of
        ClearPackageDb -> "package-db:clear"
        GlobalPackageDb -> "package-db:global"
        UserPackageDb -> "package-db:user"
        SpecificPackageDb dbPath -> "package-db:path:" <> dbPath

    renderExposureKey exposure =
      case exposure of
        ExposePackageName packageName -> "package:name:" <> packageName
        ExposePackageId packageId -> "package:id:" <> packageId

renderEnvironmentCaptureScript :: String
renderEnvironmentCaptureScript =
  "if [ -z \"${GHC_ENVIRONMENT:-}\" ]; then "
    <> "echo \"GHC_ENVIRONMENT is not set\" >&2; exit 97; fi; "
    <> "if [ ! -f \"$GHC_ENVIRONMENT\" ]; then "
    <> "echo \"GHC_ENVIRONMENT file does not exist: $GHC_ENVIRONMENT\" >&2; exit 98; fi; "
    <> "printf \""
    <> environmentPathPrefix
    <> "%s\\n\" \"$GHC_ENVIRONMENT\"; "
    <> "cat \"$GHC_ENVIRONMENT\""

runProcess' :: FilePath -> FilePath -> [String] -> IO (Either String String)
runProcess' workingDir command arguments =
  handle (\err -> pure (Left (show (err :: IOException)))) do
    (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (proc command arguments) {cwd = Just workingDir} ""
    case exitCode of
      ExitSuccess -> pure (Right stdoutText)
      ExitFailure code ->
        pure
          ( Left
              ( "Command '"
                  <> command
                  <> " "
                  <> unwords arguments
                  <> "' failed with exit code "
                  <> show code
                  <> ". Stderr: "
                  <> stderrText
              )
          )

normalizeSearchPathOutput :: String -> [FilePath]
normalizeSearchPathOutput =
  map normalise
    . filter (not . null)
    . splitSearchPath
    . trim

trim :: String -> String
trim = reverse . dropWhile isTrimChar . reverse . dropWhile isTrimChar
  where
    isTrimChar ch = isSpace ch

firstError :: String -> Either String a -> Either String a
firstError message =
  either (Left . ((message <> " ") <>) . ensureTrailingPeriod) Right

ensureTrailingPeriod :: String -> String
ensureTrailingPeriod text
  | null text = text
  | last text == '.' = text
  | otherwise = text <> "."

dedupeExposures :: [PackageExposure] -> [PackageExposure]
dedupeExposures = reverse . snd . foldl step (Set.empty, [])
  where
    step (seen, acc) exposure
      | Set.member exposure seen = (seen, acc)
      | otherwise = (Set.insert exposure seen, exposure : acc)
