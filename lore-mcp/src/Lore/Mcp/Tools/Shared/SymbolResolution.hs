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
import Control.Monad.Reader (asks)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
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
import Lore.Internal.Lookup.ModulePreference (ModulePreferenceContext (..))
import Lore.Internal.Lookup.SymbolResolutionCore
  ( ResolvedRootGroup (..),
    choosePreferredRootSymbol,
    chooseQualifierModuleName,
    collectHomeModuleNames,
    dedupeTexts,
    groupSymbolsByResolvedRoot,
    renderRootModuleName,
  )
import Lore.Mcp.Internal.LoreDoc (ToLoreDoc (toLoreDoc), paragraph)
import Lore.Session (SessionContext (customPrelude))

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
      let groupedByRoot = groupSymbolsByResolvedRoot symbolsWithRoots
      homeModuleNames <- collectHomeModuleNames
      maybeCustomPreludeName <- asks customPrelude
      let modulePreferenceContext =
            ModulePreferenceContext
              { modulePreferenceHomeModules = homeModuleNames,
                modulePreferenceCustomPrelude = GHC.mkModuleName . T.unpack <$> maybeCustomPreludeName
              }
      case groupedByRoot of
        [singleRootGroup] -> do
          let preferredSymbol =
                choosePreferredRootSymbol modulePreferenceContext singleRootGroup
          pure
            ( Right
                ResolvedSymbolQuery
                  { queryText = query,
                    parsedQuery = normalizedQuery,
                    resolvedSymbol = preferredSymbol,
                    resolvedRootName = singleRootGroup.resolvedRootName
                  }
            )
        _ -> do
          disambiguationHints <- renderDisambiguationHints query normalizedQuery modulePreferenceContext groupedByRoot
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

renderDisambiguationHints :: (MonadLore m) => Text -> NormalizedName -> ModulePreferenceContext -> [ResolvedRootGroup] -> m [Text]
renderDisambiguationHints queryText parsedQuery context groupedByRoot = do
  ownerQualifiedHints <- resolveOwnerQualifiedHints queryText (map (.resolvedRootName) groupedByRoot)
  pure $
    case parsedQuery.ownerHint of
      Just _ ->
        dedupeTexts (ownerQualifiedHints <> moduleQualifiedHints)
      Nothing ->
        if hasSingleHintModule && not (null ownerQualifiedHints)
          then ownerQualifiedHints
          else moduleQualifiedHints
  where
    hasSingleHintModule =
      length hintModules <= 1

    hintModules =
      dedupeTexts $
        map
          ( T.pack
              . Plugins.moduleNameString
              . chooseQualifierModuleName context
          )
          groupedByRoot

    moduleQualifiedHints =
      [ moduleName <> "." <> queryBaseName queryText
      | moduleName <- hintModules
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
