module Lore.Internal.Package.Materialize
  ( PackageMaterializeRunner (..),
    PackageRoot (..),
    defaultPackageMaterializeRunner,
    runHpackGeneratorWithProcess,
    materializeCabalPackageFilesIO,
    materializeCabalPackageFileIO,
    findSingleTopLevelCabalFile,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.List (intercalate, sort)
import Lore.Internal.BuildTool.Command (runProcessInWorkingDir)
import Lore.Internal.Package.Root (PackageRoot (..))
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

data PackageMaterializeRunner = PackageMaterializeRunner
  { runHpackGenerator :: FilePath -> IO (Either String ())
  }

defaultPackageMaterializeRunner :: PackageMaterializeRunner
defaultPackageMaterializeRunner =
  PackageMaterializeRunner
    { runHpackGenerator = runHpackGeneratorWithProcess runProcessInWorkingDir
    }

runHpackGeneratorWithProcess :: (FilePath -> FilePath -> [String] -> IO (Either String String)) -> FilePath -> IO (Either String ())
runHpackGeneratorWithProcess runProcess packageRoot =
  tryCandidates [] hpackCommandCandidates
  where
    tryCandidates failures [] =
      pure (Left ("Unable to run hpack using any configured command:\n" <> intercalate "\n" (map renderFailure (reverse failures))))
    tryCandidates failures (candidate@(command, arguments) : remainingCandidates) = do
      result <- runProcess packageRoot command arguments
      case result of
        Right _ -> pure (Right ())
        Left err -> tryCandidates ((candidate, err) : failures) remainingCandidates

hpackCommandCandidates :: [(FilePath, [String])]
hpackCommandCandidates =
  [ ("hpack", []),
    ("cabal", ["exec", "--", "hpack"])
  ]

renderFailure :: ((FilePath, [String]), String) -> String
renderFailure ((command, arguments), err) =
  "- " <> unwords (command : arguments) <> ": " <> err

materializeCabalPackageFilesIO ::
  PackageMaterializeRunner ->
  (String -> IO ()) ->
  (FilePath -> FilePath) ->
  [PackageRoot] ->
  IO (Either String [FilePath])
materializeCabalPackageFilesIO runner logInfo displayPath packageRoots =
  sequence <$> mapM (materializeCabalPackageFileIO runner logInfo displayPath) packageRoots

materializeCabalPackageFileIO ::
  PackageMaterializeRunner ->
  (String -> IO ()) ->
  (FilePath -> FilePath) ->
  PackageRoot ->
  IO (Either String FilePath)
materializeCabalPackageFileIO runner logInfo displayPath packageRoot = do
  let rootPath = packageRoot.packageRootPath
      packageYamlPath = rootPath </> "package.yaml"
  packageYamlExists <- doesFileExist packageYamlPath
  when packageYamlExists do
    logInfo ("Detected package.yaml in " <> displayPath rootPath <> "; running hpack before reading generated .cabal.")

  if packageYamlExists
    then do
      hpackResult <- runner.runHpackGenerator rootPath
      case hpackResult of
        Left err ->
          pure
            ( Left
                ( "Detected package.yaml in "
                    <> packageRoot.packageRootPath
                    <> ", but failed to generate a .cabal file before reading package metadata. "
                    <> err
                )
            )
        Right () ->
          resolveCabalFilePath packageRoot
    else resolveCabalFilePath packageRoot

resolveCabalFilePath :: PackageRoot -> IO (Either String FilePath)
resolveCabalFilePath packageRoot =
  case packageRoot.packageRootPreferredCabalFile of
    Just preferredCabalFile -> do
      preferredCabalFileExists <- doesFileExist preferredCabalFile
      if preferredCabalFileExists
        then pure (Right preferredCabalFile)
        else findSingleTopLevelCabalFile packageRoot.packageRootPath
    Nothing ->
      findSingleTopLevelCabalFile packageRoot.packageRootPath

findSingleTopLevelCabalFile :: FilePath -> IO (Either String FilePath)
findSingleTopLevelCabalFile packageRoot = do
  eiEntries <- try (listDirectory packageRoot) :: IO (Either IOException [FilePath])
  pure do
    entries <- firstIoError ("Failed to list package directory " <> packageRoot <> ": ") eiEntries
    let cabalFiles =
          sort
            [ packageRoot </> entry
            | entry <- entries,
              takeExtension entry == ".cabal"
            ]
    case cabalFiles of
      [single] -> Right single
      [] ->
        Left ("No .cabal file found in package directory: " <> packageRoot)
      _ ->
        Left ("Multiple .cabal files found in package directory: " <> packageRoot <> ". Use explicit package entries or remove ambiguity.")

firstIoError :: String -> Either IOException a -> Either String a
firstIoError prefix ei =
  case ei of
    Left ioErr -> Left (prefix <> show ioErr)
    Right value -> Right value
