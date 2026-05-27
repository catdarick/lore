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
import Data.Maybe (mapMaybe, maybeToList)
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
import Lore.List (minimumMaybe)
import Lore.SourceSpan (realSrcSpanFromSrcSpan)
import Lore.SourceText (readSpanLines, readSpanText)
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

data FindReferencesVerbosity
  = Low
  | Medium
  | High
  deriving stock (Eq, Show)

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

renderReferenceSnippet :: FindReferencesVerbosity -> DeclarationSpans -> [(Maybe GHC.SrcSpan, GHC.SrcSpan)] -> IO Text
renderReferenceSnippet verbosity declarationSpans referenceContexts = do
  declarationLines <- readSpanLines declarationSpans.declarationSpan
  signatureText <- traverse readSpanText declarationSpans.signatureSpan
  let bodyReferenceContexts =
        filter (not . isSignatureReference declarationSpans . snd) referenceContexts
  let selectedRanges =
        selectSnippetRanges verbosity declarationLines declarationSpans bodyReferenceContexts
      declarationSnippet =
        renderLineRanges declarationLines selectedRanges
      shouldRenderSignature =
        verbosity /= Low || null bodyReferenceContexts
  pure $
    if shouldRenderSignature
      then maybe declarationSnippet (<> "\n" <> declarationSnippet) signatureText
      else declarationSnippet

selectSnippetRanges :: FindReferencesVerbosity -> [Text] -> DeclarationSpans -> [(Maybe GHC.SrcSpan, GHC.SrcSpan)] -> [(Int, Int)]
selectSnippetRanges verbosity declarationLines declarationSpans bodyReferenceContexts =
  case verbosity of
    Low ->
      mergeLineRanges closestUsageRanges
    Medium ->
      mergeLineRanges $
        [ firstDefinitionRange 2 declarationLineCount,
          lastDefinitionRange declarationLineCount
        ]
          <> concatMap (surroundWithClosestNonEmptyLines declarationLines declarationLineCount) closestUsageRanges
    High ->
      mergeLineRanges $
        [ trimDistantDeclarationPrefix declarationLines (minimumMaybe (map fst referenceRanges)) (firstDefinitionRange 3 declarationLineCount),
          lastDefinitionRange declarationLineCount
        ]
          <> contextRanges
          <> referenceRanges
  where
    declarationLineCount = length declarationLines

    referenceRanges =
      concatMap
        (referenceSnippetRanges declarationLines declarationSpans.declarationSpan declarationLineCount . snd)
        bodyReferenceContexts

    contextRanges =
      concatMap
        ( maybe
            []
            (contextSnippetRanges declarationSpans.declarationSpan declarationLineCount)
            . fst
        )
        bodyReferenceContexts

    closestUsageRanges =
      let ranges =
            concatMap (closestUsageRangesForContext declarationSpans.declarationSpan) bodyReferenceContexts
       in if null ranges then referenceRanges else ranges

closestUsageRangesForContext :: GHC.SrcSpan -> (Maybe GHC.SrcSpan, GHC.SrcSpan) -> [(Int, Int)]
closestUsageRangesForContext declarationSpan (maybeContextSpan, referenceSpan) =
  maybeToList $
    chooseClosestUsageRange
      (referenceLineRange declarationSpan referenceSpan)
      (maybeContextSpan >>= referenceLineRange declarationSpan)

chooseClosestUsageRange :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
chooseClosestUsageRange maybeReferenceRange maybeContextRange =
  case (maybeReferenceRange, maybeContextRange) of
    (Nothing, Nothing) ->
      Nothing
    (Just referenceRange, Nothing) ->
      Just referenceRange
    (Nothing, Just contextRange) ->
      Just contextRange
    (Just referenceRange, Just contextRange) ->
      Just $
        if isCompactContextRange contextRange
          then contextRange
          else referenceRange

isCompactContextRange :: (Int, Int) -> Bool
isCompactContextRange contextRange =
  lineRangeLength contextRange <= maxLowContextLineCount

lineRangeLength :: (Int, Int) -> Int
lineRangeLength (startLine, endLine) =
  max 0 (endLine - startLine + 1)

maxLowContextLineCount :: Int
maxLowContextLineCount = 10

surroundWithClosestNonEmptyLines :: [Text] -> Int -> (Int, Int) -> [(Int, Int)]
surroundWithClosestNonEmptyLines declarationLines declarationLineCount (startLine, endLine) =
  maybeToList ((\lineNumber -> (lineNumber, lineNumber)) <$> previousNonEmptyLine declarationLines (startLine - 1))
    <> [(startLine, endLine)]
    <> maybeToList ((\lineNumber -> (lineNumber, lineNumber)) <$> nextNonEmptyLine declarationLines declarationLineCount (endLine + 1))

previousNonEmptyLine :: [Text] -> Int -> Maybe Int
previousNonEmptyLine declarationLines lineNumber
  | lineNumber < 1 =
      Nothing
  | otherwise =
      if isNonEmptyLine declarationLines lineNumber
        then Just lineNumber
        else previousNonEmptyLine declarationLines (lineNumber - 1)

nextNonEmptyLine :: [Text] -> Int -> Int -> Maybe Int
nextNonEmptyLine declarationLines declarationLineCount lineNumber
  | lineNumber > declarationLineCount =
      Nothing
  | otherwise =
      if isNonEmptyLine declarationLines lineNumber
        then Just lineNumber
        else nextNonEmptyLine declarationLines declarationLineCount (lineNumber + 1)

isNonEmptyLine :: [Text] -> Int -> Bool
isNonEmptyLine declarationLines lineNumber =
  case declarationLineText declarationLines lineNumber of
    Nothing ->
      False
    Just lineText ->
      not (T.null (T.strip lineText))

isSignatureReference :: DeclarationSpans -> GHC.SrcSpan -> Bool
isSignatureReference declarationSpans referenceSpan =
  maybe False (referenceSpan `GHC.isSubspanOf`) declarationSpans.signatureSpan

referenceLineRange :: GHC.SrcSpan -> GHC.SrcSpan -> Maybe (Int, Int)
referenceLineRange declarationSpan referenceSpan = do
  declarationRealSpan <- realSrcSpanFromSrcSpan declarationSpan
  referenceRealSpan <- realSrcSpanFromSrcSpan referenceSpan
  if GHC.srcSpanFile declarationRealSpan /= GHC.srcSpanFile referenceRealSpan
    then Nothing
    else
      let declarationStart = GHC.srcSpanStartLine declarationRealSpan
          declarationEnd = GHC.srcSpanEndLine declarationRealSpan
          referenceStart = GHC.srcSpanStartLine referenceRealSpan
          referenceEnd = GHC.srcSpanEndLine referenceRealSpan
       in if referenceEnd < declarationStart || referenceStart > declarationEnd
            then Nothing
            else
              Just
                ( referenceStart - declarationStart + 1,
                  referenceEnd - declarationStart + 1
                )

referenceSnippetRanges :: [Text] -> GHC.SrcSpan -> Int -> GHC.SrcSpan -> [(Int, Int)]
referenceSnippetRanges _ declarationSpan declarationLineCount referenceSpan =
  case referenceLineRange declarationSpan referenceSpan of
    Nothing -> []
    Just (referenceStartLine, referenceEndLine) ->
      filter
        validRange
        ( [ (max 1 (referenceStartLine - 2), max 1 (referenceStartLine - 1)),
            (referenceStartLine, min declarationLineCount (referenceStartLine + 1)),
            (max 1 (referenceEndLine - 1), referenceEndLine),
            let afterLine = min declarationLineCount (referenceEndLine + 1)
             in (afterLine, afterLine)
          ]
        )
  where
    validRange (startLine, endLine) =
      startLine <= endLine

contextSnippetRanges :: GHC.SrcSpan -> Int -> GHC.SrcSpan -> [(Int, Int)]
contextSnippetRanges declarationSpan declarationLineCount contextSpan =
  case referenceLineRange declarationSpan contextSpan of
    Nothing -> []
    Just (contextStartLine, contextEndLine)
      | contextEndLine - contextStartLine <= 8 ->
          [(contextStartLine, contextEndLine)]
      | otherwise ->
          [ (contextStartLine, min contextEndLine (contextStartLine + 1)),
            (max contextStartLine (contextEndLine - 1), min declarationLineCount contextEndLine)
          ]

lineIndentation :: [Text] -> Int -> Maybe Int
lineIndentation declarationLines lineNumber = do
  lineText <- declarationLineText declarationLines lineNumber
  let trimmedLine = T.strip lineText
  if T.null trimmedLine
    then Nothing
    else Just (T.length (T.takeWhile (== ' ') lineText))

renderReferenceBlockHeader :: DeclarationSpans -> Text
renderReferenceBlockHeader declarationSpans =
  case declarationSpansLineRange declarationSpans of
    Nothing ->
      "definition"
    Just (startLine, endLine) ->
      "lines " <> T.pack (show startLine) <> "-" <> T.pack (show endLine)

firstDefinitionRange :: Int -> Int -> (Int, Int)
firstDefinitionRange renderedLines lineCount =
  (1, min renderedLines lineCount)

trimDistantDeclarationPrefix :: [Text] -> Maybe Int -> (Int, Int) -> (Int, Int)
trimDistantDeclarationPrefix declarationLines maybeReferenceStartLine (startLine, endLine)
  | maybe False (<= endLine + 1) maybeReferenceStartLine =
      (startLine, endLine)
  | otherwise =
      (startLine, go endLine)
  where
    go currentEndLine
      | currentEndLine <= startLine = currentEndLine
      | otherwise =
          case lineIndentation declarationLines currentEndLine of
            Nothing -> currentEndLine
            Just currentIndent ->
              case minimumMaybe (mapMaybe (lineIndentation declarationLines) [startLine .. currentEndLine - 1]) of
                Just minIndent | currentIndent > minIndent -> go (currentEndLine - 1)
                _ -> currentEndLine

declarationLineText :: [Text] -> Int -> Maybe Text
declarationLineText declarationLines lineNumber =
  case compare lineNumber 1 of
    LT -> Nothing
    _ ->
      case drop (lineNumber - 1) declarationLines of
        lineText : _ -> Just lineText
        [] -> Nothing

lastDefinitionRange :: Int -> (Int, Int)
lastDefinitionRange lineCount =
  (max 1 (lineCount - 1), lineCount)

mergeLineRanges :: [(Int, Int)] -> [(Int, Int)]
mergeLineRanges ranges =
  foldr mergeRange [] (sortOn fst (filter (\(startLine, endLine) -> startLine <= endLine) ranges))
  where
    mergeRange currentRange [] =
      [currentRange]
    mergeRange (currentStart, currentEnd) ((nextStart, nextEnd) : rest)
      | currentEnd + 2 < nextStart =
          (currentStart, currentEnd) : (nextStart, nextEnd) : rest
      | otherwise =
          mergeRange (currentStart, max currentEnd nextEnd) rest

renderLineRanges :: [Text] -> [(Int, Int)] -> Text
renderLineRanges declarationLines ranges =
  case ranges of
    [] ->
      ""
    firstRange : restRanges ->
      go firstRange restRanges
  where
    go (startLine, endLine) remainingRanges =
      renderRange (startLine, endLine)
        <> case remainingRanges of
          [] ->
            ""
          nextRange@(nextStartLine, _) : tailRanges ->
            "\n"
              <> renderOmittedLines declarationLines nextStartLine (nextStartLine - endLine - 1)
              <> "\n"
              <> go nextRange tailRanges

    renderRange (startLine, endLine) =
      T.intercalate "\n" $
        take (endLine - startLine + 1) $
          drop (startLine - 1) declarationLines

    renderOmittedLines declarationLines' nextStartLine omittedLineCount
      | omittedLineCount <= 0 =
          "..."
      | otherwise =
          omittedLineIndentation declarationLines' nextStartLine <> "..."

    omittedLineIndentation declarationLines' nextStartLine =
      case declarationLineText declarationLines' nextStartLine of
        Nothing -> ""
        Just lineText -> T.takeWhile (== ' ') lineText

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
