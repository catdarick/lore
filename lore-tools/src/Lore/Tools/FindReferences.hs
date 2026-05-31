module Lore.Tools.FindReferences
  ( FindReferencesOptions (..),
    FindReferencesResult,
    FindReferencesOutput (..),
    FindReferencesFailure (..),
    FindReferencesFailureReason (..),
    FindReferencesReady (..),
    FindReferencesVerbosity (..),
    findReferences,
    renderFindReferencesFailureReason,
    renderFindReferencesOutput,
    renderFindReferencesReady,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (foldl', sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as Plugins
import Lore
  ( DeclarationSpans (..),
    DefinitionSource (..),
    MonadLore,
    NormalizedName (occName),
    NormalizedOccName,
    PathToRoot (..),
    ReferenceHit (..),
    ReferenceMatch (..),
    Symbol (..),
    parseAndNormalizeName,
    resolvePathToRoot,
    resolveReferenceMatchesForNames,
  )
import Lore.Definition.Rendering (chooseBestReferenceContext, getDefinitionSourceTree)
import Lore.SourceSpan (realSrcSpanFromSrcSpan)
import Lore.Tools.FindReferences.Snippet (renderReferenceSnippet)
import Lore.Tools.FindReferences.Types (FindReferencesVerbosity (..))
import Lore.Tools.Internal.SymbolResolution
  ( ResolvedSymbolQuery (resolvedSymbol),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
    unresolvedSymbolQueriesMessage,
  )
import Lore.Tools.Render.Doc
  ( LoreDoc,
    SourceFile (..),
    SourceSection (..),
    ToLoreDoc (toLoreDoc),
    paragraph,
    sourceFile,
  )
import Lore.Tools.Render.Source (declarationSpansLineRange, definitionSourcePath, definitionSourceRealSrcSpan)
import Lore.Tools.Render.Text (quoteText)
import Lore.Tools.Result
  ( Paginated (..),
    PaginationRenderConfig (..),
    PageRequest (..),
    PartialLoadWarning,
    ToolRun (..),
    loadedSessionPartialWarning,
    paginateItemsWithPageRequest,
    paginationSummaryDoc,
    withLoadedSession,
    withPartialLoadWarning,
  )

data FindReferencesOptions = FindReferencesOptions
  { findReferencesQuery :: Text,
    findReferencesPageRequest :: PageRequest,
    findReferencesVerbosity :: FindReferencesVerbosity
  }
  deriving stock (Eq, Show)

type FindReferencesResult = ToolRun FindReferencesOutput

data FindReferencesOutput
  = FindReferencesFailedResult FindReferencesFailure
  | FindReferencesReadyResult FindReferencesReady

data FindReferencesFailure = FindReferencesFailure
  { findReferencesFailureReason :: FindReferencesFailureReason,
    findReferencesFailurePartialLoadWarning :: Maybe PartialLoadWarning
  }

data FindReferencesFailureReason
  = FindReferencesUnresolvedSymbols SymbolsUnresolved
  | FindReferencesInternalError Text

data FindReferencesReady = FindReferencesReady
  { findReferencesSymbol :: Text,
    findReferencesPage :: Maybe (Paginated SourceFile),
    findReferencesPartialLoadWarning :: Maybe PartialLoadWarning
  }

data ReferenceOccurrenceMatch = ReferenceOccurrenceMatch
  { occurrenceMatchDefinition :: DefinitionSource,
    occurrenceMatchHit :: ReferenceHit
  }

findReferences :: (MonadLore m) => FindReferencesOptions -> m FindReferencesResult
findReferences options = do
  let targetName = parseAndNormalizeName options.findReferencesQuery
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Reference results may be incomplete."
    eiResolvedQueries <- resolveUniqueSymbolQueries [options.findReferencesQuery]
    case eiResolvedQueries of
      Left unresolvedQueries ->
        pure $
          FindReferencesFailedResult
            FindReferencesFailure
              { findReferencesFailureReason = FindReferencesUnresolvedSymbols unresolvedQueries,
                findReferencesFailurePartialLoadWarning = partialLoadWarning
              }
      Right resolved ->
        case resolved.resolvedQueries of
          [resolvedQuery] -> do
            let matchedSymbol = resolvedQuery.resolvedSymbol
            rootChain <- NE.toList . (.unPathToRoot) <$> resolvePathToRoot matchedSymbol.name
            references <- resolveReferenceMatchesForNames (filterRootChainByQuery targetName.occName [matchedSymbol] rootChain)
            let occurrenceMatches =
                  referenceMatchesToOccurrenceMatches references
            let maybeReferences =
                  paginateReferenceMatches options.findReferencesPageRequest occurrenceMatches
            case maybeReferences of
              Nothing ->
                pure $
                  FindReferencesReadyResult
                    FindReferencesReady
                      { findReferencesSymbol = options.findReferencesQuery,
                        findReferencesPage = Nothing,
                        findReferencesPartialLoadWarning = partialLoadWarning
                      }
              Just referencePagination -> do
                renderedPage <- referenceMatchesToPaginatedSourceFiles options.findReferencesVerbosity referencePagination
                pure $
                  FindReferencesReadyResult
                    FindReferencesReady
                      { findReferencesSymbol = options.findReferencesQuery,
                        findReferencesPage = Just renderedPage,
                        findReferencesPartialLoadWarning = partialLoadWarning
                      }
          _ ->
            pure $
              FindReferencesFailedResult
                FindReferencesFailure
                  { findReferencesFailureReason = FindReferencesInternalError "Internal error: expected exactly one resolved symbol query.",
                    findReferencesFailurePartialLoadWarning = partialLoadWarning
                  }

filterRootChainByQuery :: NormalizedOccName -> [Symbol] -> [GHC.Name] -> [GHC.Name]
filterRootChainByQuery targetOccName matchedSymbols rootChain =
  orderedSelectedNames
  where
    orderedSelectedNames =
      dedupeNames (matchingByOccNameOrdered <> matchingByAliasOrdered)

    matchingByOccNameOrdered =
      [ candidate
      | candidate <- rootChain,
        candidate `Set.member` matchingByOccName
      ]

    matchingByAliasOrdered =
      [ matchedSymbol.name
      | matchedSymbol <- matchedSymbols,
        matchedSymbol.name `Set.member` matchingByAlias
      ]

    matchingByOccName =
      Set.fromList
        [ candidate
        | candidate <- rootChain,
          renderOccName candidate == targetOccName
        ]

    matchingByAlias =
      Set.fromList
        [ matchedSymbol.name
        | matchedSymbol <- matchedSymbols,
          targetOccName `Set.member` matchedSymbol.aliases
        ]

    renderOccName =
      (.occName) . parseAndNormalizeName . T.pack . Plugins.getOccString

    dedupeNames =
      reverse . snd . foldl' go (Set.empty, [])

    go (seenNames, keptNames) name
      | name `Set.member` seenNames =
          (seenNames, keptNames)
      | otherwise =
          (Set.insert name seenNames, name : keptNames)

referenceMatchesToOccurrenceMatches :: [ReferenceMatch] -> [ReferenceOccurrenceMatch]
referenceMatchesToOccurrenceMatches referenceMatches =
  [ ReferenceOccurrenceMatch
      { occurrenceMatchDefinition = referenceMatch.referenceMatchDefinition,
        occurrenceMatchHit = occurrence
      }
  | referenceMatch <- referenceMatches,
    occurrence <- referenceMatch.referenceMatchOccurrences
  ]

paginateReferenceMatches :: PageRequest -> [ReferenceOccurrenceMatch] -> Maybe (Paginated ReferenceOccurrenceMatch)
paginateReferenceMatches pageRequest referenceMatches =
  paginateItemsWithPageRequest pageRequest sortedMatches
  where
    sortedMatches =
      sortOn referenceOccurrenceSortKey referenceMatches
referenceMatchesToPaginatedSourceFiles :: (MonadLore m) => FindReferencesVerbosity -> Paginated ReferenceOccurrenceMatch -> m (Paginated SourceFile)
referenceMatchesToPaginatedSourceFiles verbosity referencePagination = do
  sourceFiles <- referenceMatchesToSourceFiles verbosity referencePagination.paginatedItems
  pure
    Paginated
      { paginatedTotalItems = referencePagination.paginatedTotalItems,
        paginatedSkippedItems = referencePagination.paginatedSkippedItems,
        paginatedShownItems = referencePagination.paginatedShownItems,
        paginatedConsumedItems = referencePagination.paginatedConsumedItems,
        paginatedItems = sourceFiles
      }

referenceMatchesToSourceFiles :: (MonadLore m) => FindReferencesVerbosity -> [ReferenceOccurrenceMatch] -> m [SourceFile]
referenceMatchesToSourceFiles verbosity referenceMatches =
  mapM (referenceModuleGroupToSourceFile verbosity) (groupByModule referenceMatches)

groupByModule :: [ReferenceOccurrenceMatch] -> [[ReferenceOccurrenceMatch]]
groupByModule [] = []
groupByModule (referenceMatch : rest) =
  let (matchingModule, remaining) =
        span ((== referenceMatch.occurrenceMatchDefinition.definitionSourceModule) . (.occurrenceMatchDefinition.definitionSourceModule)) rest
   in (referenceMatch : matchingModule) : groupByModule remaining

groupByDefinition :: [ReferenceOccurrenceMatch] -> [[ReferenceOccurrenceMatch]]
groupByDefinition [] = []
groupByDefinition (referenceMatch : rest) =
  let (matchingDefinition, remaining) =
        span ((== referenceMatch.occurrenceMatchDefinition) . (.occurrenceMatchDefinition)) rest
   in (referenceMatch : matchingDefinition) : groupByDefinition remaining

referenceModuleGroupToSourceFile :: (MonadLore m) => FindReferencesVerbosity -> [ReferenceOccurrenceMatch] -> m SourceFile
referenceModuleGroupToSourceFile _ [] =
  pure
    SourceFile
      { sourceFilePath = "<definition source unavailable>",
        sourceFileSections = []
      }
referenceModuleGroupToSourceFile verbosity moduleMatches@(referenceMatch : _) = do
  renderedPath <- liftIO $ definitionSourcePath referenceMatch.occurrenceMatchDefinition
  renderedSections <- mapM (referenceMatchToSourceSection verbosity) (groupByDefinition moduleMatches)
  pure
    SourceFile
      { sourceFilePath = renderedPath,
        sourceFileSections = renderedSections
      }

referenceMatchToSourceSection :: (MonadLore m) => FindReferencesVerbosity -> [ReferenceOccurrenceMatch] -> m SourceSection
referenceMatchToSourceSection _ [] =
  pure
    SourceSection
      { sourceSectionTitle = "definition",
        sourceSectionText = ""
      }
referenceMatchToSourceSection verbosity referenceMatches@(referenceMatch : _) = do
  let declarationSpans = referenceMatch.occurrenceMatchDefinition.definitionSourceSpans
  maybeSourceTree <- getDefinitionSourceTree referenceMatch.occurrenceMatchDefinition
  let referenceContexts =
        [ ( maybeSourceTree >>= \sourceTree ->
              chooseBestReferenceContext sourceTree occurrenceMatch.occurrenceMatchHit.referenceHitExactSpan,
            occurrenceMatch.occurrenceMatchHit.referenceHitExactSpan
          )
        | occurrenceMatch <- referenceMatches
        ]
  snippetText <- liftIO $ renderReferenceSnippet verbosity declarationSpans referenceContexts
  pure
    SourceSection
      { sourceSectionTitle = renderReferenceBlockHeader declarationSpans,
        sourceSectionText = snippetText
      }

renderReferenceBlockHeader :: DeclarationSpans -> Text
renderReferenceBlockHeader declarationSpans =
  case declarationSpansLineRange declarationSpans of
    Nothing ->
      "definition"
    Just (startLine, endLine) ->
      "lines " <> T.pack (show startLine) <> "-" <> T.pack (show endLine)

referenceOccurrenceSortKey :: ReferenceOccurrenceMatch -> (String, String, Int, Int, Int, Int)
referenceOccurrenceSortKey referenceMatch =
  (moduleNameKey, filePathKey, definitionLineKey, definitionColumnKey, occurrenceLineKey, occurrenceColumnKey)
  where
    (moduleNameKey, filePathKey, definitionLineKey, definitionColumnKey) =
      definitionSourceSortKey referenceMatch.occurrenceMatchDefinition

    (occurrenceLineKey, occurrenceColumnKey) =
      case realSrcSpanFromSrcSpan referenceMatch.occurrenceMatchHit.referenceHitExactSpan of
        Just realSrcSpan ->
          ( GHC.srcSpanStartLine realSrcSpan,
            GHC.srcSpanStartCol realSrcSpan
          )
        Nothing ->
          (maxBound, maxBound)

definitionSourceSortKey :: DefinitionSource -> (String, String, Int, Int)
definitionSourceSortKey definitionSource =
  case definitionSourceRealSrcSpan definitionSource of
    Just realSrcSpan ->
      ( GHC.moduleNameString (GHC.moduleName definitionSource.definitionSourceModule),
        Plugins.unpackFS (GHC.srcSpanFile realSrcSpan),
        GHC.srcSpanStartLine realSrcSpan,
        GHC.srcSpanStartCol realSrcSpan
      )
    Nothing ->
      (GHC.moduleNameString (GHC.moduleName definitionSource.definitionSourceModule), "", maxBound, maxBound)

renderFindReferencesOutput :: FindReferencesOutput -> LoreDoc
renderFindReferencesOutput = \case
  FindReferencesFailedResult failed ->
    toLoreDoc failed
  FindReferencesReadyResult ready ->
    renderFindReferencesReady ready

instance ToLoreDoc FindReferencesOutput where
  toLoreDoc = renderFindReferencesOutput

instance ToLoreDoc FindReferencesFailure where
  toLoreDoc failed =
    withPartialLoadWarning failed.findReferencesFailurePartialLoadWarning $
      paragraph (renderFindReferencesFailureReason failed.findReferencesFailureReason)

instance ToLoreDoc FindReferencesFailureReason where
  toLoreDoc =
    paragraph . renderFindReferencesFailureReason

instance ToLoreDoc FindReferencesReady where
  toLoreDoc = renderFindReferencesReady

renderFindReferencesFailureReason :: FindReferencesFailureReason -> Text
renderFindReferencesFailureReason = \case
  FindReferencesUnresolvedSymbols unresolvedQueries ->
    unresolvedSymbolQueriesMessage unresolvedQueries
  FindReferencesInternalError message ->
    message

renderFindReferencesReady :: FindReferencesReady -> LoreDoc
renderFindReferencesReady ready =
  case ready.findReferencesPage of
    Nothing ->
      withPartialLoadWarning ready.findReferencesPartialLoadWarning $
        paragraph ("No references found for " <> quoteText ready.findReferencesSymbol <> ".")
    Just page ->
      mconcat
        [ paginationSummaryDoc
            PaginationRenderConfig
              { paginationItemLabel = "reference results",
                paginationSkipArgName = Just "skip"
              }
            page,
          mconcat (map sourceFile page.paginatedItems),
          maybe mempty toLoreDoc ready.findReferencesPartialLoadWarning
        ]
