module Lore.Tools.SearchSymbols
  ( SearchSymbolsOptions (..),
    SearchSymbolsModulePattern,
    mkSearchSymbolsModulePattern,
    searchSymbolsModulePatternText,
    SearchSymbolsResult,
    SearchSymbolsOutput (..),
    SearchSymbolsFailure (..),
    SearchSymbolsReady (..),
    searchSymbols,
    renderSearchSymbolsOutput,
    renderSearchSymbolsReady,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore (FindSimilarSymbolsOptions (..), LoreConfigError, ModulePattern, ModulePatternError, MonadLore, compileModulePattern, findSimilarSymbols, renderLoreConfigError)
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

type SearchSymbolsResult = ToolRun SearchSymbolsOutput

data SearchSymbolsOutput
  = SearchSymbolsFailed SearchSymbolsFailure
  | SearchSymbolsReadyOutput SearchSymbolsReady

newtype SearchSymbolsFailure = SearchSymbolsInvalidConfig LoreConfigError

data SearchSymbolsReady = SearchSymbolsReady
  { searchSymbolsReadyQuery :: Text,
    searchSymbolsReadyModulePatterns :: [SearchSymbolsModulePattern],
    searchSymbolsSuggestions :: [GroupedSymbolSuggestion],
    searchSymbolsPartialLoadWarning :: Maybe PartialLoadWarning
  }

searchSymbols :: (MonadLore m) => SearchSymbolsOptions -> m SearchSymbolsResult
searchSymbols options = do
  withLoadedSession \session -> do
    eiSuggestions <-
      findSimilarSymbols
        FindSimilarSymbolsOptions
          { similarSymbolsQuery = options.searchSymbolsQuery,
            similarSymbolsModulePatterns = map (.searchSymbolsCompiledModulePattern) options.searchSymbolsModulePatterns
          }
    pure $
      case eiSuggestions of
        Left configError ->
          SearchSymbolsFailed (SearchSymbolsInvalidConfig configError)
        Right suggestions ->
          let renderedSuggestions =
                maybe [] paginatedItems
                  (paginateItemsWithPageRequest
                     PageRequest
                       { pageOffset = 0,
                         pageLimit = options.searchSymbolsSuggestionLimit
                       }
                     (groupSymbolSuggestions suggestions))
           in SearchSymbolsReadyOutput
                SearchSymbolsReady
                  { searchSymbolsReadyQuery = options.searchSymbolsQuery,
                    searchSymbolsReadyModulePatterns = options.searchSymbolsModulePatterns,
                    searchSymbolsSuggestions = renderedSuggestions,
                    searchSymbolsPartialLoadWarning = loadedSessionPartialWarning session "Search results may be incomplete."
                  }

renderSearchSymbolsOutput :: SearchSymbolsOutput -> LoreDoc
renderSearchSymbolsOutput = \case
  SearchSymbolsFailed failure ->
    renderSearchSymbolsFailure failure
  SearchSymbolsReadyOutput ready ->
    renderSearchSymbolsReady ready

renderSearchSymbolsFailure :: SearchSymbolsFailure -> LoreDoc
renderSearchSymbolsFailure = \case
  SearchSymbolsInvalidConfig configError ->
    paragraph (renderLoreConfigError configError)

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
