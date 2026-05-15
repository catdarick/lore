module Lore.Mcp.Tools.FindReferences
  ( findReferencesTool,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.List (foldl', sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
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
    lookupLastLoadTargetsResult,
    parseAndNormalizeName,
    resolvePathToRoot,
    resolveReferenceMatchesForNames,
  )
import Lore.Definition.Rendering (chooseBestReferenceContext, getDefinitionSourceTree)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.List (maximumMaybe, minimumMaybe)
import Lore.Mcp.Internal.SourceSpan (realSrcSpanFromSrcSpan)
import Lore.Mcp.Internal.SourceText (readSpanLines, readSpanText, relativeSourcePath)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (PaginatedDefinitionModules (..), appendPartialLoadWarning, paginationSummaryLines)
import Lore.Mcp.Tools.Shared.SymbolResolution (ResolvedSymbolQuery (resolvedSymbol), withResolvedSymbols)
import System.Directory (getCurrentDirectory)

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

findReferencesHandler :: (MonadLore m) => FindReferencesArgs 'ValueType -> m Text
findReferencesHandler FindReferencesArgs {symbol, skip} = do
  let targetName = parseAndNormalizeName symbol
  maybeLoadResult <- lookupLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      renderedBody <-
        withResolvedSymbols [symbol] \resolvedQueries ->
          case resolvedQueries of
            [resolvedQuery] -> do
              let matchedSymbol = resolvedQuery.resolvedSymbol
              rootChain <- NE.toList . (.unPathToRoot) <$> resolvePathToRoot matchedSymbol.name
              references <- resolveReferenceMatchesForNames (filterRootChainByQuery targetName.occName [matchedSymbol] rootChain)
              renderedReferences <- renderReferenceDefinitions resolvedSkip references
              renderReferencesResult symbol renderedReferences
            _ ->
              pure "Internal error: expected exactly one resolved symbol query."
      pure (appendPartialLoadWarning loadResult "Reference results may be incomplete." renderedBody)
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

data PaginatedReferenceMatches = PaginatedReferenceMatches
  { paginationInfo :: PaginatedDefinitionModules,
    visibleMatches :: [ReferenceMatch]
  }

renderReferenceDefinitions :: (MonadLore m) => Int -> [ReferenceMatch] -> m (Maybe PaginatedReferenceMatches)
renderReferenceDefinitions skip definitionSlices =
  pure $
    case sortedMatches of
      [] ->
        Nothing
      _ ->
        Just
          PaginatedReferenceMatches
            { paginationInfo =
                PaginatedDefinitionModules
                  { totalItems = length sortedMatches,
                    skippedItems = resolvedSkip,
                    shownItems = length pagedMatches,
                    renderedPage = Nothing
                  },
              visibleMatches = pagedMatches
            }
  where
    sortedMatches =
      sortOn referenceMatchSortKey definitionSlices
    totalItems = length sortedMatches
    resolvedSkip = min (max 0 skip) totalItems
    pagedMatches =
      take maxRenderedReferenceResults (drop resolvedSkip sortedMatches)

renderReferencesResult :: (MonadLore m) => Text -> Maybe PaginatedReferenceMatches -> m Text
renderReferencesResult symbol renderedReferences =
  case renderedReferences of
    Nothing ->
      pure ("No references found for " <> quoteText symbol <> ".")
    Just paginatedReferences ->
      renderReferenceResultsPage paginatedReferences

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

referenceResultsSection :: PaginatedDefinitionModules -> [Text]
referenceResultsSection paginatedReferences =
  paginationSummaryLines "reference results" "skip" paginatedReferences
    <> maybe [] pure paginatedReferences.renderedPage

renderReferenceResultsPage :: (MonadLore m) => PaginatedReferenceMatches -> m Text
renderReferenceResultsPage paginatedReferences =
  T.intercalate "\n\n"
    . (referenceResultsSection paginatedReferences.paginationInfo <>)
    . pure
    <$> renderedReferenceMatches paginatedReferences.visibleMatches

renderedReferenceMatches :: (MonadLore m) => [ReferenceMatch] -> m Text
renderedReferenceMatches referenceMatches =
  T.intercalate "\n\n" <$> mapM renderReferenceModuleGroup (groupByModule referenceMatches)

renderReferenceModuleGroup :: (MonadLore m) => [ReferenceMatch] -> m Text
renderReferenceModuleGroup [] =
  pure ""
renderReferenceModuleGroup moduleMatches@(referenceMatch : _) = do
  renderedPath <- liftIO $ renderReferenceModulePath referenceMatch.referenceMatchDefinition
  renderedBlocks <- mapM renderReferenceMatchBlock moduleMatches
  pure $
    T.intercalate "\n\n" $
      ["=== " <> renderedPath <> " ==="]
        <> renderedBlocks

groupByModule :: [ReferenceMatch] -> [[ReferenceMatch]]
groupByModule [] = []
groupByModule (referenceMatch : rest) =
  let (matchingModule, remaining) =
        span ((== referenceMatch.referenceMatchDefinition.definitionSourceModule) . (.referenceMatchDefinition.definitionSourceModule)) rest
   in (referenceMatch : matchingModule) : groupByModule remaining

renderReferenceMatchBlock :: (MonadLore m) => ReferenceMatch -> m Text
renderReferenceMatchBlock referenceMatch = do
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
  pure $
    T.intercalate
      "\n"
      [ "--- " <> renderReferenceBlockHeader declarationSpans <> " ---",
        snippetText
      ]

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

renderReferenceModulePath :: DefinitionSource -> IO Text
renderReferenceModulePath definitionSource =
  case definitionSourceRealSrcSpan definitionSource of
    Nothing ->
      pure "<definition source unavailable>"
    Just realSrcSpan -> do
      currentDirectory <- getCurrentDirectory
      pure . T.pack $
        relativeSourcePath currentDirectory (Plugins.unpackFS (GHC.srcSpanFile realSrcSpan))

definitionSourceRealSrcSpan :: DefinitionSource -> Maybe GHC.RealSrcSpan
definitionSourceRealSrcSpan definitionSource =
  declarationSpansRealSrcSpan definitionSource.definitionSourceSpans

declarationSpansRealSrcSpan :: DeclarationSpans -> Maybe GHC.RealSrcSpan
declarationSpansRealSrcSpan declarationSpans =
  realSrcSpanFromSrcSpan declarationSpans.declarationSpan
    <|> (declarationSpans.signatureSpan >>= realSrcSpanFromSrcSpan)

declarationSpansLineRange :: DeclarationSpans -> Maybe (Int, Int)
declarationSpansLineRange declarationSpans = do
  firstSpan <- minimumMaybe realSrcSpans
  lastSpan <- maximumMaybe realSrcSpans
  pure (GHC.srcSpanStartLine firstSpan, GHC.srcSpanEndLine lastSpan)
  where
    realSrcSpans =
      mapMaybe realSrcSpanFromSrcSpan $
        maybeToList declarationSpans.signatureSpan <> [declarationSpans.declarationSpan]

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

maxRenderedReferenceResults :: Int
maxRenderedReferenceResults = 15
