module Lore.Tools.Cli.Tools.GetTypeOfExpression
  ( getTypeOfExpressionCliTool,
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
import qualified Lore.Tools.GetTypeOfExpression as GetTypeOfExpression
import Lore.Tools.Render.Doc (LoreDoc)

newtype GetTypeOfExpressionArgs = GetTypeOfExpressionArgs
  { getTypeOfExpressionInputArg :: Text
  }

getTypeOfExpressionCliTool :: CliTool LoreCliM GetTypeOfExpressionArgs
getTypeOfExpressionCliTool =
  CliTool
    { cliToolName = "type-of",
      cliToolAliases = ["type"],
      cliToolSummary = "Infer expression type",
      cliToolDescription = "Infer the type of a Haskell expression in the current interpreter context.",
      cliToolExamples =
        [ "lore-cli type-of 'map (+1) [1,2,3]'"
        ],
      cliToolArgs = getTypeOfExpressionArgs,
      cliToolRun = successfulCliToolRun runGetTypeOfExpression
    }

getTypeOfExpressionArgs :: CliArgs m GetTypeOfExpressionArgs
getTypeOfExpressionArgs =
  GetTypeOfExpressionArgs
    <$> positionalText "EXPR" "Expression to infer" noCompletion

runGetTypeOfExpression :: GetTypeOfExpressionArgs -> LoreCliM LoreDoc
runGetTypeOfExpression args = do
  result <-
    GetTypeOfExpression.getTypeOfExpression
      GetTypeOfExpression.GetTypeOfExpressionOptions
        { typeOfExpressionInput = args.getTypeOfExpressionInputArg
        }
  pure (renderToolRun GetTypeOfExpression.renderTypeExpressionOutput result)
