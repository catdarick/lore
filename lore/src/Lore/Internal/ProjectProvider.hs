module Lore.Internal.ProjectProvider
  ( ProjectProvider (..),
    detectProjectProvider,
  )
where

import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

data ProjectProvider
  = StackProject
  | CabalProject
  deriving (Eq, Ord, Show)

detectProjectProvider :: FilePath -> IO (Either String ProjectProvider)
detectProjectProvider projectRoot = do
  stackConfigExists <- doesFileExist (projectRoot </> "stack.yaml")
  if stackConfigExists
    then pure (Right StackProject)
    else do
      cabalProjectExists <- doesFileExist (projectRoot </> "cabal.project")
      if cabalProjectExists
        then pure (Right CabalProject)
        else detectFlatCabalOrHpackProject projectRoot

detectFlatCabalOrHpackProject :: FilePath -> IO (Either String ProjectProvider)
detectFlatCabalOrHpackProject projectRoot = do
  packageYamlExists <- doesFileExist (projectRoot </> "package.yaml")
  entries <- listDirectory projectRoot
  let rootCabalFiles = filter ((== ".cabal") . takeExtension) entries
  case (packageYamlExists, rootCabalFiles) of
    (True, _) ->
      pure (Right CabalProject)
    (False, [_]) ->
      pure (Right CabalProject)
    (False, []) ->
      pure
        ( Left
            "No supported project files were found. Expected one of: stack.yaml, cabal.project, package.yaml, or a single *.cabal file at the project root."
        )
    (False, _) ->
      pure
        ( Left
            "Multiple root-level *.cabal files were found without a cabal.project file. Please add cabal.project to define package selection explicitly."
        )
