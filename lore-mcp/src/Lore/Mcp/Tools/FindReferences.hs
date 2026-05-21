module Lore.Mcp.Tools.FindReferences
  ( findReferencesTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.List (foldl', sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
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
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.List (minimumMaybe)
import Lore.Mcp.Internal.LoreDoc
  ( LoreDoc,
    SourceFile (..),
    SourceSection (..),
    ToLoreDoc (toLoreDoc),
    paragraph,
    sourceFile,
  )
import Lore.Mcp.Internal.SourceSpan (realSrcSpanFromSrcSpan)
import Lore.Mcp.Internal.SourceText (readSpanLines, readSpanText)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared
  ( Paginated (..),
    PaginationRenderConfig (..),
    PartialLoadWarning (..),
    ToolRun (..),
    loadedSessionPartialWarning,
    paginateItems,
    paginationSummaryDoc,
    withLoadedSession,
    withPartialLoadWarning,
  )
import Lore.Mcp.Tools.Shared.Source (declarationSpansLineRange, definitionSourcePath, definitionSourceRealSrcSpan)
import Lore.Mcp.Tools.Shared.SymbolResolution
  ( ResolvedSymbolQuery (resolvedSymbol),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
    unresolvedSymbolQueriesMessage,
  )

data FindReferencesArgs (fieldType :: FieldType) = FindReferencesArgs
  { symbol ::
      Field fieldType Text
        `WithMeta` '[ Description "Exact symbol name to find references for. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope. For symbols ambiguous within one module (for example DuplicateRecordFields selectors), use owner qualification syntax: Some.Module.fieldName@OwnerType.",
                      Example "lookupOrZero",
                      Example "Some.Module.someFunction",
                      Example "Some.Module.fieldName@OwnerType"
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 15
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (FindReferencesArgs 'ValueType)

instance ToSchema (FindReferencesArgs 'MetadataType)

findReferencesTool :: (MonadLore m) => SomeTool m
findReferencesTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "findReferences",
        description = Just "Render all the source definitions that reference the requested symbol, including instance declarations.",
        handler = findReferencesHandler
      }

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

findReferencesHandler :: (MonadLore m) => FindReferencesArgs 'ValueType -> m FindReferencesResult
findReferencesHandler FindReferencesArgs {symbol, skip} = do
  let targetName = parseAndNormalizeName symbol
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Reference results may be incomplete."
    eiResolvedQueries <- resolveUniqueSymbolQueries [symbol]
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
            let maybeReferences =
                  paginateReferenceMatches resolvedSkip references
            case maybeReferences of
              Nothing ->
                pure $
                  FindReferencesReadyResult
                    FindReferencesReady
                      { findReferencesSymbol = symbol,
                        findReferencesPage = Nothing,
                        findReferencesPartialLoadWarning = partialLoadWarning
                      }
              Just referencePagination -> do
                renderedPage <- referenceMatchesToPaginatedSourceFiles referencePagination
                pure $
                  FindReferencesReadyResult
                    FindReferencesReady
                      { findReferencesSymbol = symbol,
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
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)

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

paginateReferenceMatches :: Int -> [ReferenceMatch] -> Maybe (Paginated ReferenceMatch)
paginateReferenceMatches skip referenceMatches =
  paginateItems resolvedSkip maxRenderedReferenceResults sortedMatches
  where
    sortedMatches =
      sortOn referenceMatchSortKey referenceMatches
    resolvedSkip = min (max 0 skip) totalItems
    totalItems = length sortedMatches

referenceMatchesToPaginatedSourceFiles :: (MonadLore m) => Paginated ReferenceMatch -> m (Paginated SourceFile)
referenceMatchesToPaginatedSourceFiles referencePagination = do
  sourceFiles <- referenceMatchesToSourceFiles referencePagination.paginatedItems
  pure
    Paginated
      { paginatedTotalItems = referencePagination.paginatedTotalItems,
        paginatedSkippedItems = referencePagination.paginatedSkippedItems,
        paginatedShownItems = referencePagination.paginatedShownItems,
        paginatedConsumedItems = referencePagination.paginatedConsumedItems,
        paginatedItems = sourceFiles
      }

referenceMatchesToSourceFiles :: (MonadLore m) => [ReferenceMatch] -> m [SourceFile]
referenceMatchesToSourceFiles referenceMatches =
  mapM referenceModuleGroupToSourceFile (groupByModule referenceMatches)

groupByModule :: [ReferenceMatch] -> [[ReferenceMatch]]
groupByModule [] = []
groupByModule (referenceMatch : rest) =
  let (matchingModule, remaining) =
        span ((== referenceMatch.referenceMatchDefinition.definitionSourceModule) . (.referenceMatchDefinition.definitionSourceModule)) rest
   in (referenceMatch : matchingModule) : groupByModule remaining

referenceModuleGroupToSourceFile :: (MonadLore m) => [ReferenceMatch] -> m SourceFile
referenceModuleGroupToSourceFile [] =
  pure
    SourceFile
      { sourceFilePath = "<definition source unavailable>",
        sourceFileSections = []
      }
referenceModuleGroupToSourceFile moduleMatches@(referenceMatch : _) = do
  renderedPath <- liftIO $ definitionSourcePath referenceMatch.referenceMatchDefinition
  renderedSections <- mapM referenceMatchToSourceSection moduleMatches
  pure
    SourceFile
      { sourceFilePath = renderedPath,
        sourceFileSections = renderedSections
      }

referenceMatchToSourceSection :: (MonadLore m) => ReferenceMatch -> m SourceSection
referenceMatchToSourceSection referenceMatch = do
  let declarationSpans = referenceMatch.referenceMatchDefinition.definitionSourceSpans
  maybeSourceTree <- getDefinitionSourceTree referenceMatch.referenceMatchDefinition
  let referenceContexts =
        [ ( maybeSourceTree >>= \sourceTree ->
              chooseBestReferenceContext sourceTree occurrence.referenceHitExactSpan,
            occurrence.referenceHitExactSpan
          )
        | occurrence <- referenceMatch.referenceMatchOccurrences
        ]
  snippetText <- liftIO $ renderReferenceSnippet declarationSpans referenceContexts
  pure
    SourceSection
      { sourceSectionTitle = renderReferenceBlockHeader declarationSpans,
        sourceSectionText = snippetText
      }

renderReferenceSnippet :: DeclarationSpans -> [(Maybe GHC.SrcSpan, GHC.SrcSpan)] -> IO Text
renderReferenceSnippet declarationSpans referenceContexts = do
  declarationLines <- readSpanLines declarationSpans.declarationSpan
  signatureText <- traverse readSpanText declarationSpans.signatureSpan
  let bodyReferenceContexts =
        filter (not . isSignatureReference declarationSpans . snd) referenceContexts
  let referenceRanges =
        concatMap
          (referenceSnippetRanges declarationLines declarationSpans.declarationSpan (length declarationLines) . snd)
          bodyReferenceContexts
      contextRanges =
        concatMap
          ( maybe
              []
              (contextSnippetRanges declarationSpans.declarationSpan (length declarationLines))
              . fst
          )
          bodyReferenceContexts
  let selectedRanges =
        mergeLineRanges $
          [ trimDistantDeclarationPrefix declarationLines (minimumMaybe (map fst referenceRanges)) (firstDefinitionRange (length declarationLines)),
            lastDefinitionRange (length declarationLines)
          ]
            <> contextRanges
            <> referenceRanges
      declarationSnippet =
        renderLineRanges declarationLines selectedRanges
  pure $
    maybe declarationSnippet (<> "\n" <> declarationSnippet) signatureText

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

firstDefinitionRange :: Int -> (Int, Int)
firstDefinitionRange lineCount =
  (1, min 3 lineCount)

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
    _ -> case drop (lineNumber - 1) declarationLines of
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

referenceMatchSortKey :: ReferenceMatch -> (String, String, Int, Int)
referenceMatchSortKey referenceMatch =
  case definitionSourceRealSrcSpan referenceMatch.referenceMatchDefinition of
    Just realSrcSpan ->
      ( GHC.moduleNameString (GHC.moduleName referenceMatch.referenceMatchDefinition.definitionSourceModule),
        Plugins.unpackFS (GHC.srcSpanFile realSrcSpan),
        GHC.srcSpanStartLine realSrcSpan,
        GHC.srcSpanStartCol realSrcSpan
      )
    Nothing ->
      (GHC.moduleNameString (GHC.moduleName referenceMatch.referenceMatchDefinition.definitionSourceModule), "", maxBound, maxBound)

instance ToLoreDoc FindReferencesOutput where
  toLoreDoc = \case
    FindReferencesFailedResult failed ->
      toLoreDoc failed
    FindReferencesReadyResult ready ->
      renderFindReferencesReady ready

instance ToLoreDoc FindReferencesFailure where
  toLoreDoc failed =
    withPartialLoadWarning failed.findReferencesFailurePartialLoadWarning $
      paragraph (renderFindReferencesFailureReason failed.findReferencesFailureReason)

instance ToLoreDoc FindReferencesFailureReason where
  toLoreDoc =
    paragraph . renderFindReferencesFailureReason

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

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

maxRenderedReferenceResults :: Int
maxRenderedReferenceResults = 15
