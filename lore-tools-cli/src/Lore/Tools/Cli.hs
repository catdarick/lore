module Lore.Tools.Cli
  ( runCli,
  )
where

import Lore
  ( runLore,
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
import System.Exit (exitFailure)

runCli :: IO ()
runCli = do
  options <- execParser (parserInfo cliTools)
  case options.cliMode of
    CliSingle format invocation -> do
      status <- runLore (sessionConfigForInvocation invocation) (runSingleShot format invocation)
      case status of
        CliInvocationSucceeded -> pure ()
        CliInvocationFailed -> exitFailure
    CliInteractive format ->
      runLore interactiveSessionConfig (runInteractive cliTools format)
