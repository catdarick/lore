module Lore.Tools.LookupSymbolInfo
  ( LookupSymbolInfoOptions (..),
    LookupSymbolInfoResult,
    LookupSymbolInfoReady (..),
    lookupSymbolInfo,
    renderLookupSymbolInfoReady,
  )
where

import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC.Plugins as Plugins
import Lore (FindSimilarSymbolsOptions (..), MonadLore, PathToRoot (..), Symbol (..), SymbolInfo (..), findMatchingSymbols, findSimilarSymbols, listDirectInstances, parseAndNormalizeName, resolvePathToRoot)
import qualified Lore as Core
import Lore.Tools.Internal.DetailedSymbolInfo (DetailedSymbolInfo (..), detailedSymbolInfoLabel)
import Lore.Tools.Internal.SymbolSuggestions
  ( GroupedSymbolSuggestion,
    groupSymbolSuggestions,
    groupedSymbolSuggestionLabel,
    noSymbolsFound,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc), numberedListFrom, paragraph)
import Lore.Tools.Result
  ( Paginated (..),
    PaginationRenderConfig (..),
    PageRequest (..),
    PartialLoadWarning,
    ResultLimit (..),
    ToolRun,
    loadedSessionPartialWarning,
    paginateItemsWithPageRequest,
    paginationSummaryDoc,
    withLoadedSession,
  )

data LookupSymbolInfoOptions = LookupSymbolInfoOptions
  { lookupSymbolInfoQuery :: Text,
    lookupSymbolInfoPageRequest :: PageRequest,
    lookupSymbolInfoSuggestionLimit :: ResultLimit
  }
  deriving stock (Eq, Show)

type LookupSymbolInfoResult = ToolRun LookupSymbolInfoReady

data LookupSymbolInfoReady = LookupSymbolInfoReady
  { lookupSymbolInfoReadyQuery :: Text,
    lookupSymbolInfoPage :: Maybe (Paginated DetailedSymbolInfo),
    lookupSymbolInfoSuggestions :: [GroupedSymbolSuggestion],
    lookupSymbolInfoPartialLoadWarning :: Maybe PartialLoadWarning
  }

lookupSymbolInfo :: (MonadLore m) => LookupSymbolInfoOptions -> m LookupSymbolInfoResult
lookupSymbolInfo options = do
  withLoadedSession \session -> do
    symbolInfos <- lookupExactSymbolInfos options.lookupSymbolInfoQuery
    let partialLoadWarning =
          loadedSessionPartialWarning session "Symbol lookup results may be incomplete."
    case symbolInfos of
      [] -> do
        suggestions <-
          findSimilarSymbols
            FindSimilarSymbolsOptions
              { similarSymbolsQuery = options.lookupSymbolInfoQuery,
                similarSymbolsModulePatterns = []
              }
        let renderedSuggestions =
              maybe [] paginatedItems
                (paginateItemsWithPageRequest
                   PageRequest
                     { pageOffset = 0,
                       pageLimit = options.lookupSymbolInfoSuggestionLimit
                     }
                   (groupSymbolSuggestions suggestions))
        pure
          LookupSymbolInfoReady
            { lookupSymbolInfoReadyQuery = options.lookupSymbolInfoQuery,
              lookupSymbolInfoPage = Nothing,
              lookupSymbolInfoSuggestions = renderedSuggestions,
              lookupSymbolInfoPartialLoadWarning = partialLoadWarning
            }
      _ -> do
        detailedSymbolInfos <- mapM mkDetailedSymbolInfo symbolInfos
        pure
          LookupSymbolInfoReady
            { lookupSymbolInfoReadyQuery = options.lookupSymbolInfoQuery,
              lookupSymbolInfoPage = paginateDetailedSymbolInfos options.lookupSymbolInfoPageRequest detailedSymbolInfos,
              lookupSymbolInfoSuggestions = [],
              lookupSymbolInfoPartialLoadWarning = partialLoadWarning
            }

renderLookupSymbolInfoReady :: LookupSymbolInfoReady -> LoreDoc
renderLookupSymbolInfoReady ready =
  case (ready.lookupSymbolInfoPage, ready.lookupSymbolInfoSuggestions) of
    (Nothing, []) ->
      mconcat
        [ paragraph (noSymbolsFound ready.lookupSymbolInfoReadyQuery),
          maybe mempty toLoreDoc ready.lookupSymbolInfoPartialLoadWarning
        ]
    (Nothing, suggestions) ->
      mconcat
        [ paragraph (noSymbolsFound ready.lookupSymbolInfoReadyQuery <> " Maybe you meant one of these?"),
          numberedListFrom 1 (map (paragraph . groupedSymbolSuggestionLabel) suggestions),
          maybe mempty toLoreDoc ready.lookupSymbolInfoPartialLoadWarning
        ]
    (Just page, _) ->
      mconcat
        [ paginationSummaryDoc
            PaginationRenderConfig
              { paginationItemLabel = "symbol candidates",
                paginationSkipArgName = Just "skip"
              }
            page,
          numberedListFrom (fromIntegral (page.paginatedSkippedItems + 1)) (map (paragraph . detailedSymbolInfoLabel) page.paginatedItems),
          maybe mempty toLoreDoc ready.lookupSymbolInfoPartialLoadWarning
        ]

paginateDetailedSymbolInfos :: PageRequest -> [DetailedSymbolInfo] -> Maybe (Paginated DetailedSymbolInfo)
paginateDetailedSymbolInfos pageRequest =
  paginateItemsWithPageRequest pageRequest

lookupExactSymbolInfos :: (MonadLore m) => Text -> m [SymbolInfo]
lookupExactSymbolInfos query = do
  matchedSymbols <- Set.toList <$> findMatchingSymbols (parseAndNormalizeName query)
  preferredSymbolsByRoot <- pickClosestSymbolsToRoot matchedSymbols
  catMaybes <$> mapM (Core.lookupSymbolInfo . (.name)) preferredSymbolsByRoot

pickClosestSymbolsToRoot :: (MonadLore m) => [Symbol] -> m [Symbol]
pickClosestSymbolsToRoot symbols = do
  symbolsWithPath <- mapM resolveSymbolPathToRoot symbols
  pure $
    map (\(symbol, _, _) -> symbol) $
      Map.elems $
        foldl' keepClosestByRoot Map.empty symbolsWithPath
  where
    keepClosestByRoot acc current@(_, pathToRoot, _) =
      Map.insertWith
        preferClosestToRoot
        (rootDedupKey (NE.last pathToRoot))
        current
        acc

    preferClosestToRoot new@(_, _, newDistance) old@(_, _, oldDistance)
      | oldDistance <= newDistance = old
      | otherwise = new

    rootDedupKey name =
      ( fmap (Plugins.moduleNameString . Plugins.moduleName) (Plugins.nameModule_maybe name),
        Plugins.getOccString name
      )

resolveSymbolPathToRoot :: (MonadLore m) => Symbol -> m (Symbol, NE.NonEmpty Plugins.Name, Int)
resolveSymbolPathToRoot symbol = do
  PathToRoot pathToRoot <- resolvePathToRoot symbol.name
  pure (symbol, pathToRoot, max 0 (length pathToRoot - 1))

mkDetailedSymbolInfo :: (MonadLore m) => SymbolInfo -> m DetailedSymbolInfo
mkDetailedSymbolInfo symbolInfo = do
  instancesInfo <- listDirectInstances (symbolName symbolInfo)
  pure
    DetailedSymbolInfo
      { symbolInfo,
        instancesInfo
      }
