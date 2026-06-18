module Lore.Tools.Cli
  ( runCli,
  )
where

import qualified Data.Text as T
import Lore
  ( loadStartupConfig,
    renderSessionConfigError,
    runLore,
    startupSessionConfig,
  )
import Lore.Tools.Cli.Interactive (runInteractive)
import Lore.Tools.Cli.Internal.Parser
  ( CliMode (..),
    CliOptions (..),
    parserInfo,
  )
import Lore.Tools.Cli.Internal.Tool (CliInvocationStatus (..))
import Lore.Tools.Cli.Registry (cliTools)
import Lore.Tools.Cli.SingleShot (runSingleShot)
import Options.Applicative (execParser)
import System.Exit (exitFailure)

runCli :: IO ()
runCli = do
  options <- execParser (parserInfo cliTools)
  baseSessionConfig <-
    startupSessionConfig <$> (loadStartupConfig >>= either failWithSessionConfigError pure)
  case options.cliMode of
    CliSingle format invocation -> do
      status <- runLore baseSessionConfig (runSingleShot format invocation)
      case status of
        CliInvocationSucceeded -> pure ()
        CliInvocationFailed -> exitFailure
    CliInteractive format ->
      runLore baseSessionConfig (runInteractive cliTools format)
  where
    failWithSessionConfigError =
      ioError . userError . T.unpack . renderSessionConfigError
