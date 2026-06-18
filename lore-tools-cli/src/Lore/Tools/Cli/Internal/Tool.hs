module Lore.Tools.Cli.Internal.Tool
  ( LoreCliM,
    CliTool (..),
    SomeCliTool (..),
    CliInvocation (..),
    CliInvocationResult (..),
    CliInvocationStatus (..),
    successfulCliToolRun,
    runCliInvocation,
    cliInvocationName,
  )
where

import Data.Text (Text)
import Lore (LoreMonadT)
import Lore.Tools.Cli.Internal.Annotated (CliArgs)
import Lore.Tools.Render.Doc (LoreDoc)

type LoreCliM = LoreMonadT IO

data CliTool m args = CliTool
  { cliToolName :: Text,
    cliToolAliases :: [Text],
    cliToolSummary :: Text,
    cliToolDescription :: Text,
    cliToolExamples :: [Text],
    cliToolArgs :: CliArgs m args,
    cliToolRun :: args -> m CliInvocationResult
  }

data CliInvocationResult = CliInvocationResult
  { cliInvocationResultDoc :: LoreDoc,
    cliInvocationResultStatus :: CliInvocationStatus
  }

data CliInvocationStatus
  = CliInvocationSucceeded
  | CliInvocationFailed
  deriving stock (Eq, Show)

data SomeCliTool m where
  SomeCliTool :: CliTool m args -> SomeCliTool m

data CliInvocation m where
  CliInvocation :: CliTool m args -> args -> CliInvocation m

successfulCliToolRun :: (Functor m) => (args -> m LoreDoc) -> args -> m CliInvocationResult
successfulCliToolRun run args =
  toSuccessfulResult <$> run args

toSuccessfulResult :: LoreDoc -> CliInvocationResult
toSuccessfulResult loreDoc =
  CliInvocationResult
    { cliInvocationResultDoc = loreDoc,
      cliInvocationResultStatus = CliInvocationSucceeded
    }

runCliInvocation :: CliInvocation m -> m CliInvocationResult
runCliInvocation (CliInvocation tool args) =
  cliToolRun tool args

cliInvocationName :: CliInvocation m -> Text
cliInvocationName (CliInvocation tool _args) =
  cliToolName tool
