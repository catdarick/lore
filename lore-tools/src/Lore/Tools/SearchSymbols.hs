module Lore.Tools.SearchSymbols
  ( SearchSymbolsOptions (..),
    SearchSymbolsModulePattern,
    mkSearchSymbolsModulePattern,
    searchSymbolsModulePatternText,
    SearchSymbolsResult,
    SearchSymbolsReady (..),
    searchSymbols,
    renderSearchSymbolsReady,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore (FindSimilarSymbolsOptions (..), ModulePattern, ModulePatternError, MonadLore, compileModulePattern, findSimilarSymbols, parseAndNormalizeName)
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
    searchSymbolsSuggestionLimit :: ResultLimit,
    searchSymbolsModulePatterns :: [SearchSymbolsModulePattern]
  }
  deriving stock (Eq, Show)

data SearchSymbolsModulePattern = SearchSymbolsModulePattern
  { searchSymbolsModulePatternText :: Text,
    searchSymbolsCompiledModulePattern :: ModulePattern
  }
  deriving stock (Eq, Show)

mkSearchSymbolsModulePattern :: Text -> Either ModulePatternError SearchSymbolsModulePattern
mkSearchSymbolsModulePattern rawPattern = do
  compiledPattern <- compileModulePattern rawPattern
  pure
    SearchSymbolsModulePattern
      { searchSymbolsModulePatternText = rawPattern,
        searchSymbolsCompiledModulePattern = compiledPattern
      }

type SearchSymbolsResult = ToolRun SearchSymbolsReady

data SearchSymbolsReady = SearchSymbolsReady
  { searchSymbolsReadyQuery :: Text,
    searchSymbolsReadyModulePatterns :: [SearchSymbolsModulePattern],
    searchSymbolsSuggestions :: [GroupedSymbolSuggestion],
    searchSymbolsPartialLoadWarning :: Maybe PartialLoadWarning
  }

searchSymbols :: (MonadLore m) => SearchSymbolsOptions -> m SearchSymbolsResult
searchSymbols options = do
  withLoadedSession \session -> do
    suggestions <-
      findSimilarSymbols
        FindSimilarSymbolsOptions
          { similarSymbolsLimit = suggestionFetchLimit options.searchSymbolsSuggestionLimit,
            similarSymbolsModulePatterns = map (.searchSymbolsCompiledModulePattern) options.searchSymbolsModulePatterns
          }
        (parseAndNormalizeName options.searchSymbolsQuery)
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
          searchSymbolsReadyModulePatterns = options.searchSymbolsModulePatterns,
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
        [ paragraph (noSymbolsFound ready.searchSymbolsReadyQuery <> renderModulePatternScopeSuffix "." ready.searchSymbolsReadyModulePatterns),
          maybe mempty toLoreDoc ready.searchSymbolsPartialLoadWarning
        ]
    suggestions ->
      mconcat
        [ paragraph ("Found " <> T.pack (show (length suggestions)) <> " similar symbols for " <> quoteText ready.searchSymbolsReadyQuery <> renderModulePatternScopeSuffix ":" ready.searchSymbolsReadyModulePatterns),
          numberedListFrom 1 (map (paragraph . groupedSymbolSuggestionLabel) suggestions),
          maybe mempty toLoreDoc ready.searchSymbolsPartialLoadWarning
        ]

renderModulePatternScopeSuffix :: Text -> [SearchSymbolsModulePattern] -> Text
renderModulePatternScopeSuffix terminal modulePatterns =
  case modulePatterns of
    [] ->
      terminal
    [modulePattern] ->
      " in modules matching " <> quoteText modulePattern.searchSymbolsModulePatternText <> terminal
    _ ->
      " in modules matching any of:\n" <> T.intercalate ", " (map (quoteText . (.searchSymbolsModulePatternText)) modulePatterns) <> terminal
