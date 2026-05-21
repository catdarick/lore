module Lore.Mcp.Tools.LookupSymbolInfo
  ( lookupSymbolInfoTool,
  )
where

import qualified Data.Aeson as J
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified GHC.Plugins as Plugins
import Lore (MonadLore, PathToRoot (..), Symbol (..), SymbolInfo (..), findMatchingSymbols, findSimilarSymbols, listDirectInstances, lookupSymbolInfo, parseAndNormalizeName, resolvePathToRoot)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.LoreDoc (ToLoreDoc (toLoreDoc), numberedListFrom, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared
  ( Paginated (..),
    PaginationRenderConfig (..),
    PartialLoadWarning,
    ToolRun (..),
    loadedSessionPartialWarning,
    paginateItems,
    paginationSummaryDoc,
    withLoadedSession,
  )
import Lore.Mcp.Tools.Shared.DetailedSymbolInfo (DetailedSymbolInfo (..), detailedSymbolInfoLabel)
import Lore.Mcp.Tools.Shared.SymbolSuggestions
  ( GroupedSymbolSuggestion,
    groupSymbolSuggestions,
    groupedSymbolSuggestionLabel,
    maxRenderedSymbolSuggestions,
    noSymbolsFound,
    symbolSuggestionFetchLimit,
  )

data LookupSymbolInfoArgs (fieldType :: FieldType) = LookupSymbolInfoArgs
  { symbol ::
      Field fieldType Text
        `WithMeta` '[ Description "Symbol name to look up. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      Example "lookupOrZero",
                      Example "Some.Module.someFunction"
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 5
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (LookupSymbolInfoArgs 'ValueType)

instance ToSchema (LookupSymbolInfoArgs 'MetadataType)

type LookupSymbolInfoResult = ToolRun LookupSymbolInfoReady

data LookupSymbolInfoReady = LookupSymbolInfoReady
  { lookupSymbolInfoQuery :: Text,
    lookupSymbolInfoPage :: Maybe (Paginated DetailedSymbolInfo),
    lookupSymbolInfoSuggestions :: [GroupedSymbolSuggestion],
    lookupSymbolInfoPartialLoadWarning :: Maybe PartialLoadWarning
  }

instance ToLoreDoc LookupSymbolInfoReady where
  toLoreDoc ready =
    case (ready.lookupSymbolInfoPage, ready.lookupSymbolInfoSuggestions) of
      (Nothing, []) ->
        mconcat
          [ paragraph (noSymbolsFound ready.lookupSymbolInfoQuery),
            maybe mempty toLoreDoc ready.lookupSymbolInfoPartialLoadWarning
          ]
      (Nothing, suggestions) ->
        mconcat
          [ paragraph (noSymbolsFound ready.lookupSymbolInfoQuery <> " Maybe you meant one of these?"),
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

lookupSymbolInfoTool :: (MonadLore m) => SomeTool m
lookupSymbolInfoTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "lookupSymbolInfo",
        description =
          Just
            "Look up metadata and information for a Haskell symbol in the current session. \
            \Supports module-qualified queries and semantic fuzzy matching. \
            \Note on scope: Unexported top-level symbols are available for home modules. For package modules, exported symbols remain visible even if a home-module load fails (provided a load was attempted). \
            \During partial loads, 'No symbols found' only means the symbol isn't in the loaded session; it does not prove it is absent from the source.",
        handler = lookupSymbolInfoHandler
      }

lookupSymbolInfoHandler :: (MonadLore m) => LookupSymbolInfoArgs 'ValueType -> m LookupSymbolInfoResult
lookupSymbolInfoHandler LookupSymbolInfoArgs {symbol, skip} = do
  withLoadedSession \session -> do
    symbolInfos <- lookupExactSymbolInfos symbol
    let partialLoadWarning =
          loadedSessionPartialWarning session "Symbol lookup results may be incomplete."
    case symbolInfos of
      [] -> do
        suggestions <- findSimilarSymbols symbolSuggestionFetchLimit (parseAndNormalizeName symbol)
        pure
          LookupSymbolInfoReady
            { lookupSymbolInfoQuery = symbol,
              lookupSymbolInfoPage = Nothing,
              lookupSymbolInfoSuggestions = take maxRenderedSymbolSuggestions (groupSymbolSuggestions suggestions),
              lookupSymbolInfoPartialLoadWarning = partialLoadWarning
            }
      _ -> do
        detailedSymbolInfos <- mapM mkDetailedSymbolInfo symbolInfos
        pure
          LookupSymbolInfoReady
            { lookupSymbolInfoQuery = symbol,
              lookupSymbolInfoPage = paginateDetailedSymbolInfos resolvedSkip detailedSymbolInfos,
              lookupSymbolInfoSuggestions = [],
              lookupSymbolInfoPartialLoadWarning = partialLoadWarning
            }
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)

paginateDetailedSymbolInfos :: Int -> [DetailedSymbolInfo] -> Maybe (Paginated DetailedSymbolInfo)
paginateDetailedSymbolInfos skip =
  paginateItems skip maxRenderedSymbolCandidates

maxRenderedSymbolCandidates :: Int
maxRenderedSymbolCandidates = 5

lookupExactSymbolInfos :: (MonadLore m) => Text -> m [SymbolInfo]
lookupExactSymbolInfos query = do
  matchedSymbols <- Set.toList <$> findMatchingSymbols (parseAndNormalizeName query)
  preferredSymbolsByRoot <- pickClosestSymbolsToRoot matchedSymbols
  catMaybes <$> mapM (lookupSymbolInfo . (.name)) preferredSymbolsByRoot

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
