module Lore.Tools.Cli.SingleShot
  ( runSingleShot,
    sessionConfigForInvocation,
    interactiveSessionConfig,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Lore
  ( SessionConfig (..),
    defaultSessionConfig,
  )
import Lore.Tools.Cli.Internal.Parser (OutputFormat)
import Lore.Tools.Cli.Internal.Tool
  ( CliInvocation,
    CliInvocationResult (..),
    CliInvocationStatus,
    SessionRequirements (..),
    cliInvocationName,
    cliInvocationSessionRequirements,
    runCliInvocation,
  )
import Lore.Tools.Cli.Render (renderOutput)

runSingleShot :: (MonadIO m) => OutputFormat -> CliInvocation m -> m CliInvocationStatus
runSingleShot format invocation = do
  invocationResult <- runCliInvocation invocation
  renderOutput format (cliInvocationName invocation) invocationResult.cliInvocationResultDoc
  pure invocationResult.cliInvocationResultStatus

sessionConfigForInvocation :: CliInvocation m -> SessionConfig
sessionConfigForInvocation invocation =
  let requirements = cliInvocationSessionRequirements invocation
   in
  defaultSessionConfig
    { isTestSuiteFunctionalityRequired =
        requirements.requiresTestSuiteFunctionality
    }

interactiveSessionConfig :: SessionConfig
interactiveSessionConfig =
  defaultSessionConfig
    { isTestSuiteFunctionalityRequired = True
    }
