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
        else detectProviderFromRootCabalFile projectRoot

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
