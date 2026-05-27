module Lore.Tools.Pagination
  ( ToolPolicy (..),
    mcpDefaultToolPolicy,
    limitToIntWithDefault,
    limitToMaybeInt,
  )
where

import Lore.Tools.Result (ResultLimit (..))

data ToolPolicy = ToolPolicy
  { definitionLimit :: ResultLimit,
    referenceLimit :: ResultLimit,
    deadCodeLimit :: ResultLimit,
    exportedSymbolsLimit :: ResultLimit,
    symbolCandidatesLimit :: ResultLimit,
    symbolSuggestionsLimit :: ResultLimit,
    instanceLimit :: ResultLimit,
    diagnosticsLimit :: ResultLimit,
    directoryEntryBudget :: ResultLimit
  }
  deriving stock (Eq, Show)

mcpDefaultToolPolicy :: ToolPolicy
mcpDefaultToolPolicy =
  ToolPolicy
    { definitionLimit = Limit 30,
      referenceLimit = Limit 15,
      deadCodeLimit = Limit 100,
      exportedSymbolsLimit = Limit 150,
      symbolCandidatesLimit = Limit 5,
      symbolSuggestionsLimit = Limit 10,
      instanceLimit = Limit 25,
      diagnosticsLimit = Limit 5,
      directoryEntryBudget = Limit 150
    }

limitToIntWithDefault :: Int -> ResultLimit -> Int
limitToIntWithDefault fallback = \case
  Unlimited ->
    fallback
  Limit value ->
    max 0 value

limitToMaybeInt :: ResultLimit -> Maybe Int
limitToMaybeInt = \case
  Unlimited ->
    Nothing
  Limit value ->
    Just (max 0 value)
