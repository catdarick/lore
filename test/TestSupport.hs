module TestSupport (fixtureLore) where

import Control.Exception (bracket)
import Internal.Logger (noLogHandle)
import Lore (runLoreMonadT)
import Monad (LoreMonadT)
import Session (defaultSessionConfig)
import qualified Session
import System.Directory (makeAbsolute)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))

fixtureLore :: LoreMonadT IO a -> IO a
fixtureLore action = do
  fixtureRoot <- makeAbsolute ("test" </> "fixtures" </> "demo")
  withClearedGhcEnvironment $
    runLoreMonadT
      defaultSessionConfig
        { Session.projectRoot = fixtureRoot,
          Session.ghcWorkDir = fixtureRoot </> ".lore-work-test",
          Session.loggerHandle = noLogHandle
        }
      action

withClearedGhcEnvironment :: IO a -> IO a
withClearedGhcEnvironment action =
  bracket (lookupEnv "GHC_ENVIRONMENT" <* unsetEnv "GHC_ENVIRONMENT") restore (const action)
  where
    restore =
      maybe (pure ()) (setEnv "GHC_ENVIRONMENT")
