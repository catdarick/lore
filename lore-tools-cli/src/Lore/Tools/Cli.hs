module Lore.Tools.Cli
  ( runCli,
  )
where

import Lore
  ( loadSessionConfigFromEnvironment,
    renderSessionConfigError,
    runLore,
  )
import Lore.Tools.Cli.Interactive (runInteractive)
import Lore.Tools.Cli.Internal.Tool (CliInvocationStatus (..))
import Lore.Tools.Cli.Internal.Parser
  ( CliMode (..),
    CliOptions (..),
    parserInfo,
  )
import Lore.Tools.Cli.Registry (cliTools)
import Lore.Tools.Cli.SingleShot
  ( interactiveSessionConfig,
    runSingleShot,
    sessionConfigForInvocation,
  )
import Options.Applicative (execParser)
import qualified Data.Text as T
import System.Exit (exitFailure)

runCli :: IO ()
runCli = do
  options <- execParser (parserInfo cliTools)
  baseSessionConfig <-
    loadSessionConfigFromEnvironment >>= either failWithSessionConfigError pure
  case options.cliMode of
    CliSingle format invocation -> do
      status <- runLore (sessionConfigForInvocation baseSessionConfig invocation) (runSingleShot format invocation)
      case status of
        CliInvocationSucceeded -> pure ()
        CliInvocationFailed -> exitFailure
    CliInteractive format ->
      runLore (interactiveSessionConfig baseSessionConfig) (runInteractive cliTools format)
  where
    failWithSessionConfigError =
      ioError . userError . T.unpack . renderSessionConfigError
