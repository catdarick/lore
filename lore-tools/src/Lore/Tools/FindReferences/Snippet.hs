module Lore.Tools.FindReferences.Snippet
  ( renderReferenceSnippet,
  )
where

import Data.List (sortOn)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import Lore (DeclarationSpans (..))
import Lore.List (minimumMaybe)
import Lore.SourceSpan (realSrcSpanFromSrcSpan)
import Lore.SourceText (readSpanLines, readSpanText)
import Lore.Tools.FindReferences.Types (FindReferencesVerbosity (..))

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
