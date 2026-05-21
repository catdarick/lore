module Lore.Mcp.Tools.Shared.SymbolResolution
  ( ResolvedSymbolQuery (..),
    UnresolvedSymbolQuery (..),
    SymbolsResolved (..),
    SymbolsUnresolved (..),
    SymbolResolutionResult (..),
    resolveUniqueSymbolQueries,
    resolveSymbolQueries,
    unresolvedSymbolQueriesMessage,
  )
where

import Control.Monad (filterM)
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as Plugins
import Lore
  ( MonadLore,
    NormalizedName (ownerHint),
    PathToRoot (..),
    Symbol (..),
    findMatchingSymbols,
    parseAndNormalizeName,
    resolvePathToRoot,
  )
import Lore.Mcp.Internal.LoreDoc (ToLoreDoc (toLoreDoc), paragraph)

data ResolvedSymbolQuery = ResolvedSymbolQuery
  { queryText :: !Text,
    parsedQuery :: !NormalizedName,
    resolvedSymbol :: !Symbol,
    resolvedRootName :: !Plugins.Name
  }

newtype SymbolsResolved = MkSymbolsResolved
  { resolvedQueries :: [ResolvedSymbolQuery]
  }

data UnresolvedSymbolQuery
  = UnresolvedSymbolQuery'Missing !Text
  | UnresolvedSymbolQuery'Ambiguous !Text ![Text]

newtype SymbolsUnresolved = MkSymbolsUnresolved
  { unresolvedQueries :: [UnresolvedSymbolQuery]
  }

data SymbolResolutionResult
  = SymbolQueriesResolved SymbolsResolved
  | SymbolQueriesUnresolved SymbolsUnresolved

resolveUniqueSymbolQueries :: (MonadLore m) => [Text] -> m (Either SymbolsUnresolved SymbolsResolved)
resolveUniqueSymbolQueries queries = do
  resolutions <- mapM resolveOneQuery queries
  let unresolved = [unresolvedQuery | Left unresolvedQuery <- resolutions]
  pure
    if null unresolved
      then Right (MkSymbolsResolved [resolvedQuery | Right resolvedQuery <- resolutions])
      else Left (MkSymbolsUnresolved unresolved)

resolveSymbolQueries :: (MonadLore m) => [Text] -> m SymbolResolutionResult
resolveSymbolQueries queries = do
  eiResolved <- resolveUniqueSymbolQueries queries
  pure $ case eiResolved of
    Left unresolved -> SymbolQueriesUnresolved unresolved
    Right resolved -> SymbolQueriesResolved resolved

resolveOneQuery :: (MonadLore m) => Text -> m (Either UnresolvedSymbolQuery ResolvedSymbolQuery)
resolveOneQuery query = do
  let normalizedQuery = parseAndNormalizeName query
  matchingSymbols <- Set.toList <$> findMatchingSymbols normalizedQuery
  case matchingSymbols of
    [] ->
      pure (Left (UnresolvedSymbolQuery'Missing query))
    [symbol] -> do
      rootName <- resolveRootName symbol.name
      pure
        ( Right
            ResolvedSymbolQuery
              { queryText = query,
                parsedQuery = normalizedQuery,
                resolvedSymbol = symbol,
                resolvedRootName = rootName
              }
        )
    _ -> do
      symbolsWithRoots <- mapM resolveSymbolRoot matchingSymbols
      let rootNames = dedupeNamesBy renderName (map snd symbolsWithRoots)
      case rootNames of
        [singleRootName] ->
          case symbolsWithRoots of
            (preferredSymbol, _) : _ ->
              pure
                ( Right
                    ResolvedSymbolQuery
                      { queryText = query,
                        parsedQuery = normalizedQuery,
                        resolvedSymbol = preferredSymbol,
                        resolvedRootName = singleRootName
                      }
                )
            [] ->
              pure (Left (UnresolvedSymbolQuery'Missing query))
        _ -> do
          disambiguationHints <- renderDisambiguationHints query normalizedQuery matchingSymbols rootNames
          pure (Left (UnresolvedSymbolQuery'Ambiguous query disambiguationHints))

resolveSymbolRoot :: (MonadLore m) => Symbol -> m (Symbol, Plugins.Name)
resolveSymbolRoot symbol = do
  rootName <- resolveRootName symbol.name
  pure (symbol, rootName)

resolveRootName :: (MonadLore m) => Plugins.Name -> m Plugins.Name
resolveRootName name =
  NE.last . (.unPathToRoot) <$> resolvePathToRoot name

instance ToLoreDoc SymbolsUnresolved where
  toLoreDoc =
    paragraph . unresolvedSymbolQueriesMessage

unresolvedSymbolQueriesMessage :: SymbolsUnresolved -> Text
unresolvedSymbolQueriesMessage unresolvedQueries =
  T.intercalate "\n\n" (map renderUnresolvedSymbolQuery unresolvedQueries.unresolvedQueries)

renderUnresolvedSymbolQuery :: UnresolvedSymbolQuery -> Text
renderUnresolvedSymbolQuery unresolvedQuery =
  case unresolvedQuery of
    UnresolvedSymbolQuery'Missing queryText ->
      "No symbols found for " <> quoteText queryText <> "."
    UnresolvedSymbolQuery'Ambiguous queryText disambiguationHints ->
      T.intercalate
        "\n"
        ( [ "The requested name " <> quoteText queryText <> " is ambiguous. More qualification is required:",
            ""
          ]
            <> map ("  - " <>) disambiguationHints
            <> ["", "Run the tool again with a fully qualified symbol name from the list above."]
        )

renderDisambiguationHints :: (MonadLore m) => Text -> NormalizedName -> [Symbol] -> [Plugins.Name] -> m [Text]
renderDisambiguationHints queryText parsedQuery matchedSymbols matchedRoots = do
  ownerQualifiedHints <- resolveOwnerQualifiedHints queryText matchedRoots
  pure $
    case parsedQuery.ownerHint of
      Just _ ->
        dedupeTexts (ownerQualifiedHints <> moduleQualifiedHints)
      Nothing ->
        if hasSingleDefinitionModule && not (null ownerQualifiedHints)
          then ownerQualifiedHints
          else moduleQualifiedHints
  where
    hasSingleDefinitionModule =
      length definitionModules <= 1

    definitionModules =
      dedupeTexts (map renderSymbolModuleName matchedSymbols)

    moduleQualifiedHints =
      [ moduleName <> "." <> queryBaseName queryText
      | moduleName <- definitionModules
      ]

resolveOwnerQualifiedHints :: (MonadLore m) => Text -> [Plugins.Name] -> m [Text]
resolveOwnerQualifiedHints queryText rootNames = do
  ownerHintCandidates <-
    filterM
      ownerHintResolvesUniquely
      [ renderRootModuleName rootName <> "." <> queryBaseName queryText <> "@" <> T.pack (Plugins.getOccString rootName)
      | rootName <- rootNames
      ]
  pure (dedupeTexts ownerHintCandidates)

ownerHintResolvesUniquely :: (MonadLore m) => Text -> m Bool
ownerHintResolvesUniquely query = do
  maybeResolution <- resolveUniqueSymbolQueries [query]
  pure $ case maybeResolution of
    Right (MkSymbolsResolved [_]) -> True
    _ -> False

renderName :: Plugins.Name -> String
renderName name =
  case Plugins.nameModule_maybe name of
    Nothing ->
      "<no-module>." <> Plugins.getOccString name
    Just module_ ->
      Plugins.moduleNameString (Plugins.moduleName module_) <> "." <> Plugins.getOccString name

renderSymbolModuleName :: Symbol -> Text
renderSymbolModuleName symbol =
  renderRootModuleName symbol.name

renderRootModuleName :: Plugins.Name -> Text
renderRootModuleName name =
  case Plugins.nameModule_maybe name of
    Nothing ->
      "<no-module>"
    Just module_ ->
      T.pack (Plugins.moduleNameString (Plugins.moduleName module_))

queryBaseName :: Text -> Text
queryBaseName queryText =
  case reverse (T.splitOn "." (stripOwnerHintSuffix queryText)) of
    occNameText : _
      | not (T.null occNameText) ->
          occNameText
    _ ->
      stripOwnerHintSuffix queryText

stripOwnerHintSuffix :: Text -> Text
stripOwnerHintSuffix queryText =
  case T.breakOnEnd "@" queryText of
    ("", _) ->
      queryText
    (prefixWithDelimiter, ownerHintText)
      | T.null ownerHintText ->
          queryText
      | otherwise ->
          T.dropEnd 1 prefixWithDelimiter

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

dedupeNamesBy :: (Ord k) => (a -> k) -> [a] -> [a]
dedupeNamesBy renderKey =
  reverse . snd . foldl' collectUnique (Set.empty, [])
  where
    collectUnique (seenKeys, dedupedNames) name =
      let key = renderKey name
       in if key `Set.member` seenKeys
            then (seenKeys, dedupedNames)
            else (Set.insert key seenKeys, name : dedupedNames)

dedupeTexts :: [Text] -> [Text]
dedupeTexts =
  dedupeNamesBy id
