module Lore.Tools.Cli.Tools.Common
  ( offsetArg,
    limitArg,
    directoryBudgetArg,
    noArgs,
    resultLimitToInt,
    resultLimitToMaybeInt,
    renderToolRun,
    noCompletion,
    staticCompletionValues,
  )
where

import Data.Text (Text)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs (..),
    CompletionItem (..),
    CompletionProvider (..),
    optionWithReader,
  )
import Lore.Tools.Cli.Internal.Parser
  ( renderResultLimit,
    resultLimitReader,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.Result (ResultLimit (..), ToolRun (..))
import Options.Applicative (ReadM, eitherReader)

offsetArg :: CliArgs m Int
offsetArg =
  optionWithReader
    auto
    "offset"
    Nothing
    "N"
    "Result offset"
    Nothing
    (Just 0)
    noCompletion

limitArg :: CliArgs m ResultLimit
limitArg =
  optionWithReader
    resultLimitReader
    "limit"
    Nothing
    "N|unlimited"
    "Result limit"
    (Just renderResultLimit)
    (Just Unlimited)
    (staticCompletionValues ["10", "30", "100", "unlimited"])

directoryBudgetArg :: CliArgs m ResultLimit
directoryBudgetArg =
  optionWithReader
    resultLimitReader
    "directory-budget"
    Nothing
    "N|unlimited"
    "Directory traversal budget"
    (Just renderResultLimit)
    (Just Unlimited)
    (staticCompletionValues ["50", "150", "500", "unlimited"])

noArgs :: CliArgs m ()
noArgs =
  CliArgs
    { cliArgsParser = pure (),
      cliArgsSpecs = []
    }

resultLimitToInt :: ResultLimit -> Int
resultLimitToInt = \case
  Unlimited -> maxBound
  Limit limit -> max 0 limit

resultLimitToMaybeInt :: ResultLimit -> Maybe Int
resultLimitToMaybeInt = \case
  Unlimited -> Nothing
  Limit limit -> Just (max 0 limit)

renderToolRun :: (ready -> LoreDoc) -> ToolRun ready -> LoreDoc
renderToolRun renderReady = \case
  ToolRunBlocked blocked -> toLoreDoc blocked
  ToolRunReady ready -> renderReady ready

noCompletion :: CompletionProvider m
noCompletion = NoCompletion

staticCompletionValues :: [Text] -> CompletionProvider m
staticCompletionValues values =
  StaticCompletion
    [ CompletionItem
        { completionInsert = value,
          completionDisplay = value,
          completionHelp = Nothing
        }
      | value <- values
    ]

auto :: (Read a) => ReadM a
auto =
  eitherReader \raw ->
    case reads raw of
      [(value, "")] -> Right value
      _ -> Left "invalid value"
