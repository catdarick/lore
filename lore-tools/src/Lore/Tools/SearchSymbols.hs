module Lore.Tools.SearchSymbols
  ( SearchSymbolsOptions (..),
    SearchSymbolsResult,
    SearchSymbolsReady (..),
    searchSymbols,
    renderSearchSymbolsReady,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore (MonadLore, findSimilarSymbols, parseAndNormalizeName)
import Lore.Tools.Internal.SymbolSuggestions
  ( GroupedSymbolSuggestion,
    groupSymbolSuggestions,
    groupedSymbolSuggestionLabel,
    noSymbolsFound,
    quoteText,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc), numberedListFrom, paragraph)
import Lore.Tools.Result
  ( Paginated (..),
    PageRequest (..),
    PartialLoadWarning,
    ResultLimit (..),
    ToolRun,
    loadedSessionPartialWarning,
    paginateItemsWithPageRequest,
    withLoadedSession,
  )

data SearchSymbolsOptions = SearchSymbolsOptions
  { searchSymbolsQuery :: Text,
    searchSymbolsSuggestionLimit :: ResultLimit
  }
  deriving stock (Eq, Show)

type SearchSymbolsResult = ToolRun SearchSymbolsReady

data SearchSymbolsReady = SearchSymbolsReady
  { searchSymbolsReadyQuery :: Text,
    searchSymbolsSuggestions :: [GroupedSymbolSuggestion],
    searchSymbolsPartialLoadWarning :: Maybe PartialLoadWarning
  }

searchSymbols :: (MonadLore m) => SearchSymbolsOptions -> m SearchSymbolsResult
searchSymbols options = do
  withLoadedSession \session -> do
    suggestions <- findSimilarSymbols (suggestionFetchLimit options.searchSymbolsSuggestionLimit) (parseAndNormalizeName options.searchSymbolsQuery)
    let renderedSuggestions =
          maybe [] paginatedItems
            (paginateItemsWithPageRequest
               PageRequest
                 { pageOffset = 0,
                   pageLimit = options.searchSymbolsSuggestionLimit
                 }
               (groupSymbolSuggestions suggestions))
    pure
      SearchSymbolsReady
        { searchSymbolsReadyQuery = options.searchSymbolsQuery,
          searchSymbolsSuggestions = renderedSuggestions,
          searchSymbolsPartialLoadWarning = loadedSessionPartialWarning session "Search results may be incomplete."
        }

suggestionFetchLimit :: ResultLimit -> Int
suggestionFetchLimit = \case
  Unlimited ->
    maxBound
  Limit limit ->
    max 1 (safeMultiply 20 (max 1 limit))
  where
    safeMultiply left right
      | left == 0 || right == 0 = 0
      | left > maxBound `div` right = maxBound
      | otherwise = left * right

renderSearchSymbolsReady :: SearchSymbolsReady -> LoreDoc
renderSearchSymbolsReady ready =
  case ready.searchSymbolsSuggestions of
    [] ->
      mconcat
        [ paragraph (noSymbolsFound ready.searchSymbolsReadyQuery),
          maybe mempty toLoreDoc ready.searchSymbolsPartialLoadWarning
        ]
    suggestions ->
      mconcat
        [ paragraph ("Found " <> T.pack (show (length suggestions)) <> " similar symbols for " <> quoteText ready.searchSymbolsReadyQuery <> ":"),
          numberedListFrom 1 (map (paragraph . groupedSymbolSuggestionLabel) suggestions),
          maybe mempty toLoreDoc ready.searchSymbolsPartialLoadWarning
        ]
