module Lore.Bench.FixtureProject
  ( FixtureProject (..),
    fixtureProjectRoot,
    fixturePackageFiles,
    withFixtureLore,
  )
where

import Control.Exception (bracket)
import Lore (LoreMonadT)
import Lore.Logger (noLogHandle)
import Lore.Session (defaultSessionConfig, runLore)
import qualified Lore.Session as Session
import System.Directory (makeAbsolute)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))

data FixtureProject
  = SmallFixture
  | MediumFixture
  deriving stock (Eq, Show)

fixtureProjectRoot :: FixtureProject -> IO FilePath
fixtureProjectRoot fixture =
  makeAbsolute ("bench-fixtures" </> fixtureDir fixture)
  where
    fixtureDir = \case
      SmallFixture -> "lore-small"
      MediumFixture -> "lore-medium"

fixturePackageFiles :: FixtureProject -> IO [FilePath]
fixturePackageFiles fixture = do
  root <- fixtureProjectRoot fixture
  pure [root </> "package.yaml", root </> "stack.yaml"]

withFixtureLore :: FixtureProject -> LoreMonadT IO a -> IO a
withFixtureLore fixture action =
  withClearedGhcEnvironment do
    root <- fixtureProjectRoot fixture
    runLore
      defaultSessionConfig
        { Session.projectRoot = root,
          Session.ghcWorkDir = root </> ".lore-bench-work",
          Session.loggerHandle = noLogHandle
        }
      action

withClearedGhcEnvironment :: IO a -> IO a
withClearedGhcEnvironment action =
  bracket (lookupEnv "GHC_ENVIRONMENT" <* unsetEnv "GHC_ENVIRONMENT") restore (const action)
  where
    restore = maybe (pure ()) (setEnv "GHC_ENVIRONMENT")
