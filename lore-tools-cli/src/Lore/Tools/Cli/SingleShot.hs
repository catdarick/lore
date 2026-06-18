module Lore.Tools.Cli.SingleShot
  ( runSingleShot,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Lore.Tools.Cli.Internal.Parser (OutputFormat)
import Lore.Tools.Cli.Internal.Tool
  ( CliInvocation,
    CliInvocationResult (..),
    CliInvocationStatus,
    cliInvocationName,
    runCliInvocation,
  )
import Lore.Tools.Cli.Render (renderOutput)

runSingleShot :: (MonadIO m) => OutputFormat -> CliInvocation m -> m CliInvocationStatus
runSingleShot format invocation = do
  invocationResult <- runCliInvocation invocation
  renderOutput format (cliInvocationName invocation) invocationResult.cliInvocationResultDoc
  pure invocationResult.cliInvocationResultStatus
