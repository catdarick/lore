module Lore.Tools.Cli.Tools.ExecuteCode
  ( executeCodeCliTool,
  )
where

import Data.Text (Text)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    positionalText,
  )
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common (noCompletion, renderToolRun)
import qualified Lore.Tools.ExecuteCode as ExecuteCode
import Lore.Tools.Render.Doc (LoreDoc)

newtype ExecuteCodeArgs = ExecuteCodeArgs
  { executeCodeInputArg :: Text
  }

executeCodeCliTool :: CliTool LoreCliM ExecuteCodeArgs
executeCodeCliTool =
  CliTool
    { cliToolName = "exec",
      cliToolAliases = ["run"],
      cliToolSummary = "Execute one-line expression",
      cliToolDescription = "Execute a one-line expression or IO action in the interpreter context.",
      cliToolExamples =
        [ "lore-cli exec 'print (1 + 2)'"
        ],
      cliToolArgs = executeCodeArgs,
      cliToolRun = successfulCliToolRun runExecuteCode
    }

executeCodeArgs :: CliArgs m ExecuteCodeArgs
executeCodeArgs =
  ExecuteCodeArgs
    <$> positionalText "EXPR" "Expression or IO action" noCompletion

runExecuteCode :: ExecuteCodeArgs -> LoreCliM LoreDoc
runExecuteCode args = do
  result <-
    ExecuteCode.executeCode
      ExecuteCode.ExecuteCodeOptions
        { executeCodeInput = args.executeCodeInputArg
        }
  pure (renderToolRun ExecuteCode.renderExecuteCode result)
