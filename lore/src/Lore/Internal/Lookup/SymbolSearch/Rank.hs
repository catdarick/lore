module Lore.Internal.Lookup.SymbolSearch.Rank
  ( parseSymbolSearchQuery,
    findSymbolSearchSuggestions,
    tokenIdf,
    scoreSymbolDocument,
    wholeNameDistance,
  )
where

import Data.Char (isLetter, isUpper)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Types.Name as GHC
import Lore.Internal.Lookup.ModulePattern (ModulePattern, matchesModulePattern)
import Lore.Internal.Lookup.Name (NormalizedOccName, parseQualifiedNormalizedOccName, unNormalizedOccName)
import Lore.Internal.Lookup.SymbolSearch.Index (fieldTokenSequences)
import Lore.Internal.Lookup.SymbolSearch.Match (candidateNamesFromMatches, findContiguousTermOccurrences, matchQueryTerms, queryTokenIsFirstOccurrence)
import Lore.Internal.Lookup.SymbolSearch.Synonyms (SynonymLexicon, SynonymTerm (..))
import Lore.Internal.Lookup.SymbolSearch.Tokenize (tokenizeSearchText)
import Lore.Internal.Lookup.SymbolSearch.Types
  ( IndexedNameVariant (..),
    IndexedTokenSequence (..),
    QueryTermMatch (..),
    SearchToken (..),
    StoredMatchPattern (..),
    SymbolScoreBreakdown (..),
    SymbolSearchDocument (..),
    SymbolSearchField (..),
    SymbolSearchIndex (..),
    SymbolSearchQuery (..),
    TermMatchEvidence (..),
    TokenMatchKind (..),
    TokenSpan (..),
  )
import Lore.Internal.Lookup.Types (Symbol (name), SymbolSuggestion (..))
import qualified Text.EditDistance as EditDistance

parseSymbolSearchQuery :: Text -> SymbolSearchQuery
parseSymbolSearchQuery rawQuery =
  SymbolSearchQuery
    { symbolSearchText = occName.unNormalizedOccName,
      symbolSearchTokens = tokenizeSearchText occName.unNormalizedOccName,
      symbolSearchExactModule = exactModule
    }
  where
    (exactModule, occName) = parseQualifiedNormalizedOccName rawQuery

findSymbolSearchSuggestions :: SynonymLexicon -> [ModulePattern] -> Text -> SymbolSearchIndex -> [SymbolSuggestion]
findSymbolSearchSuggestions lexicon modulePatterns rawQuery index =
  List.sortOn suggestionSortKey $
    mapMaybe (scoreCandidate query tokenMatches) candidateDocuments
  where
    query = parseSymbolSearchQuery rawQuery
    tokenMatches = matchQueryTerms lexicon index query.symbolSearchTokens
    candidateNames =
      Set.filter
        (candidateInScope index query modulePatterns)
        (candidateNamesFromMatches index tokenMatches)
    candidateDocuments =
      mapMaybe (`Map.lookup` index.searchDocuments) (Set.toList candidateNames)

    scoreCandidate parsedQuery matches document =
      case scoreSymbolDocument index parsedQuery matches document of
        Nothing -> Nothing
        Just (lookupName, exactLookupMatch, score, evidence) ->
          Just
            SymbolSuggestion
              { suggestedSymbol = document.symbolSearchSymbol,
                suggestedLookupName = lookupName.unNormalizedOccName,
                suggestionExactLookupNameMatch = exactLookupMatch,
                suggestionScore = score,
                suggestionEvidence = evidence
              }

    suggestionSortKey suggestion =
      ( Down suggestion.suggestionExactLookupNameMatch,
        Down suggestion.suggestionScore,
        wholeNameDistance query.symbolSearchText suggestion.suggestedLookupName,
        suggestion.suggestedLookupName,
        suggestion.suggestedSymbol.name
      )

candidateInScope :: SymbolSearchIndex -> SymbolSearchQuery -> [ModulePattern] -> GHC.Name -> Bool
candidateInScope index query modulePatterns symbolName =
  case Map.lookup symbolName index.searchDocuments of
    Nothing -> False
    Just document ->
      exactModuleMatches document && modulePatternsMatch document
  where
    exactModuleMatches document =
      case query.symbolSearchExactModule of
        Nothing -> True
        Just exactModule -> exactModule `Set.member` document.symbolSearchModules
    modulePatternsMatch document =
      null modulePatterns
        || any
          (\moduleName -> any (`matchesModulePattern` moduleName) modulePatterns)
          (Set.toList document.symbolSearchModules)

scoreSymbolDocument :: SymbolSearchIndex -> SymbolSearchQuery -> [QueryTermMatch] -> SymbolSearchDocument -> Maybe (NormalizedOccName, Bool, Double, [TermMatchEvidence])
scoreSymbolDocument index query matches document =
  case scoredVariants of
    [] -> Nothing
    _ ->
      let bestVariant = List.minimumBy scoredVariantOrdering scoredVariants
       in Just (bestVariant.scoredVariantName, bestVariant.scoredVariantExactLookupNameMatch, bestVariant.scoredVariantScore, bestVariant.scoredVariantEvidence)
  where
    scoredVariants =
      mapMaybe (scoreNameVariant index query matches document) (NE.toList document.symbolSearchNames)

data ScoredNameVariant = ScoredNameVariant
  { scoredVariantName :: NormalizedOccName,
    scoredVariantExactLookupNameMatch :: Bool,
    scoredVariantWholeNameDistance :: Int,
    scoredVariantScore :: Double,
    scoredVariantEvidence :: [TermMatchEvidence]
  }

scoreNameVariant :: SymbolSearchIndex -> SymbolSearchQuery -> [QueryTermMatch] -> SymbolSearchDocument -> IndexedNameVariant -> Maybe ScoredNameVariant
scoreNameVariant index query matches document nameVariant =
  case selectedEvidence of
    [] | not exactLookupNameMatch -> Nothing
    _ ->
      Just
        ScoredNameVariant
          { scoredVariantName = nameVariant.indexedName,
            scoredVariantExactLookupNameMatch = exactLookupNameMatch,
            scoredVariantWholeNameDistance = wholeNameDistance query.symbolSearchText nameVariant.indexedName.unNormalizedOccName,
            scoredVariantScore = scoreBreakdownTotal breakdown,
            scoredVariantEvidence = selectedEvidence
          }
  where
    selectedEvidence =
      selectionEvidence selection
    evidenceOptions =
      concatMap (evidenceForMatch index document nameVariant) matches
    selection =
      selectEvidence query nameVariant evidenceOptions
    unmatchedCount =
      selectionUnmatchedTokenCount selection
    exactLookupNameMatch =
      T.toLower nameVariant.indexedName.unNormalizedOccName == T.toLower query.symbolSearchText
    breakdown =
      SymbolScoreBreakdown
        { matchedEvidenceScore = sum (map (.evidenceContribution) selectedEvidence),
          unmatchedTokenPenalty = unmatchedTokenPenaltyWeight * fromIntegral unmatchedCount,
          orderedNameBonus = orderedNameBonusFor query nameVariant selectedEvidence,
          nameSpecificityBonus = nameSpecificityBonusFor nameVariant selectedEvidence,
          capitalizationPenalty = capitalizationMismatchPenalty query.symbolSearchText nameVariant.indexedName.unNormalizedOccName
        }

scoredVariantOrdering :: ScoredNameVariant -> ScoredNameVariant -> Ordering
scoredVariantOrdering =
  comparing
    ( \variant ->
        ( Down variant.scoredVariantExactLookupNameMatch,
          Down variant.scoredVariantScore,
          variant.scoredVariantWholeNameDistance,
          variant.scoredVariantName
        )
    )

data StoredTermOccurrence = StoredTermOccurrence
  { occurrenceField :: SymbolSearchField,
    occurrenceStoredTokens :: NE.NonEmpty SearchToken,
    occurrenceStoredSpan :: TokenSpan,
    occurrenceSourceSequence :: NE.NonEmpty SearchToken,
    occurrenceNameVariant :: Maybe NormalizedOccName
  }

evidenceForMatch :: SymbolSearchIndex -> SymbolSearchDocument -> IndexedNameVariant -> QueryTermMatch -> [TermMatchEvidence]
evidenceForMatch index document nameVariant match =
  [ mkEvidence occurrence
  | occurrence <- storedOccurrences document nameVariant match.matchedStoredPattern
  ]
  where
    mkEvidence occurrence =
      let idf = matchTermIdf index occurrence.occurrenceField match occurrence
       in TermMatchEvidence
            { evidenceQuerySpan = match.matchedQuerySpan,
              evidenceQueryTokens = match.matchedQueryTokens,
              evidenceStoredTokens = occurrence.occurrenceStoredTokens,
              evidenceStoredSpan = occurrence.occurrenceStoredSpan,
              evidenceSourceSequence = occurrence.occurrenceSourceSequence,
              evidenceField = occurrence.occurrenceField,
              evidenceNameVariant = occurrence.occurrenceNameVariant,
              evidenceMatchKind = match.matchedKind,
              evidenceEditDistance = match.matchedEditDistance,
              evidenceMatchQuality = match.matchedQuality,
              evidenceIdf = idf,
              evidenceContribution = fromIntegral match.matchedQuerySpan.tokenSpanLength * fieldWeight occurrence.occurrenceField * match.matchedQuality * idf
            }

storedOccurrences :: SymbolSearchDocument -> IndexedNameVariant -> StoredMatchPattern -> [StoredTermOccurrence]
storedOccurrences document nameVariant pattern_ =
  nameOccurrences <> contextOccurrences
  where
    nameOccurrences =
      occurrencesInSequence SearchName (Just nameVariant.indexedName) (IndexedTokenSequence nameVariant.indexedNameTokens) pattern_
    contextOccurrences =
      [ occurrence
      | field <- [SearchResultType, SearchArgumentType, SearchModule],
        tokenSequence <- fieldTokenSequences field document,
        occurrence <- occurrencesInSequence field Nothing tokenSequence pattern_
      ]

occurrencesInSequence :: SymbolSearchField -> Maybe NormalizedOccName -> IndexedTokenSequence -> StoredMatchPattern -> [StoredTermOccurrence]
occurrencesInSequence field nameVariant (IndexedTokenSequence sequenceTokens) pattern_ =
  case pattern_ of
    StoredTokenPattern token ->
      [ StoredTermOccurrence
          { occurrenceField = field,
            occurrenceStoredTokens = storedToken NE.:| [],
            occurrenceStoredSpan = TokenSpan position 1,
            occurrenceSourceSequence = sequenceTokens,
            occurrenceNameVariant = nameVariant
          }
      | (position, storedToken) <- zip [0 ..] (NE.toList sequenceTokens),
        storedToken == token
      ]
    StoredSynonymTermPattern (SynonymTerm termTokens) ->
      [ StoredTermOccurrence
          { occurrenceField = field,
            occurrenceStoredTokens = sliceNonEmpty span_ sequenceTokens,
            occurrenceStoredSpan = span_,
            occurrenceSourceSequence = sequenceTokens,
            occurrenceNameVariant = nameVariant
          }
      | span_ <- findContiguousTermOccurrences sequenceTokens termTokens
      ]

sliceNonEmpty :: TokenSpan -> NE.NonEmpty a -> NE.NonEmpty a
sliceNonEmpty span_ tokens =
  case NE.nonEmpty (take span_.tokenSpanLength (drop span_.tokenSpanStart (NE.toList tokens))) of
    Just values -> values
    Nothing -> error "non-empty token span produced empty slice"

data EvidenceSelection = EvidenceSelection
  { selectionEvidence :: [TermMatchEvidence],
    selectionMatchedScore :: Double,
    selectionUnmatchedTokenCount :: Int
  }

selectEvidence :: SymbolSearchQuery -> IndexedNameVariant -> [TermMatchEvidence] -> EvidenceSelection
selectEvidence query nameVariant evidence =
  bestFor 0 0 True
  where
    queryTokens = query.symbolSearchTokens
    evidenceByStart =
      Map.fromListWith
        (<>)
        [ (item.evidenceQuerySpan.tokenSpanStart, [item])
        | item <- evidence
        ]
    queryLength = length queryTokens
    nameTokenCount = length (NE.toList nameVariant.indexedNameTokens)
    bestByState =
      LazyMap.fromList
        [ ((position, nextNameStart, orderedSoFar), bestAt position nextNameStart orderedSoFar)
        | position <- [0 .. queryLength],
          nextNameStart <- [0 .. nameTokenCount],
          orderedSoFar <- [False, True]
        ]

    bestFor position nextNameStart orderedSoFar =
      LazyMap.findWithDefault (mkSelection []) (position, min nextNameStart nameTokenCount, orderedSoFar) bestByState

    bestAt position nextNameStart orderedSoFar
      | position >= queryLength =
          mkSelection []
      | otherwise =
          List.minimumBy (selectionOrdering query nameVariant) (ignoreCurrent : evidenceTransitions)
      where
        ignoreRest = bestFor (position + 1) nextNameStart orderedSoFar
        ignoreCurrent = ignoreRest
        evidenceTransitions =
          [ addEvidence item (bestFor (spanEnd item.evidenceQuerySpan) nextNameStart' orderedSoFar')
          | item <- Map.findWithDefault [] position evidenceByStart,
            nameEvidenceAllowed nextNameStart orderedSoFar item,
            let (nextNameStart', orderedSoFar') = advanceNameOrderState nextNameStart orderedSoFar item
          ]

    addEvidence item rest =
      mkSelection (item : filter (not . storedSpansOverlap item) rest.selectionEvidence)

    mkSelection selectedEvidence =
      EvidenceSelection
        { selectionEvidence = selectedEvidence,
          selectionMatchedScore = sum (map (.evidenceContribution) selectedEvidence),
          selectionUnmatchedTokenCount = unmatchedTokenCountFor queryTokens selectedEvidence
        }

advanceNameOrderState :: Int -> Bool -> TermMatchEvidence -> (Int, Bool)
advanceNameOrderState nextNameStart orderedSoFar item
  | item.evidenceField /= SearchName =
      (nextNameStart, orderedSoFar)
  | item.evidenceStoredSpan.tokenSpanStart >= nextNameStart =
      (spanEnd item.evidenceStoredSpan, orderedSoFar)
  | otherwise =
      (nextNameStart, False)

nameEvidenceAllowed :: Int -> Bool -> TermMatchEvidence -> Bool
nameEvidenceAllowed nextNameStart orderedSoFar item =
  item.evidenceField /= SearchName
    || not orderedSoFar
    || item.evidenceStoredSpan.tokenSpanStart >= nextNameStart

selectionOrdering :: SymbolSearchQuery -> IndexedNameVariant -> EvidenceSelection -> EvidenceSelection -> Ordering
selectionOrdering query nameVariant =
  comparing
    ( \selection ->
        ( Down (selectionFinalScore query nameVariant selection),
          Down (selectionMatchedPositionCount selection),
          Down (selectionExactCanonicalCount selection),
          Down (selectionNameFieldCount selection),
          Down (selectionPhraseLength selection),
          length selection.selectionEvidence,
          selectionEvidenceKey selection
        )
    )

selectionFinalScore :: SymbolSearchQuery -> IndexedNameVariant -> EvidenceSelection -> Double
selectionFinalScore query nameVariant selection =
  selection.selectionMatchedScore
    - unmatchedTokenPenaltyWeight * fromIntegral selection.selectionUnmatchedTokenCount
    + orderedNameBonusFor query nameVariant selection.selectionEvidence
    + nameSpecificityBonusFor nameVariant selection.selectionEvidence

unmatchedTokenCountFor :: [SearchToken] -> [TermMatchEvidence] -> Int
unmatchedTokenCountFor queryTokens evidence =
  length
    [ ()
    | (position, _token) <- zip [0 ..] queryTokens,
      queryTokenIsFirstOccurrence queryTokens position,
      not (position `Set.member` matchedPositions)
    ]
  where
    matchedPositions =
      Set.fromList
        [ position
        | item <- evidence,
          position <- [item.evidenceQuerySpan.tokenSpanStart .. spanEnd item.evidenceQuerySpan - 1]
        ]

storedSpansOverlap :: TermMatchEvidence -> TermMatchEvidence -> Bool
storedSpansOverlap left right =
  sameStoredSource left right
    && spansOverlap left.evidenceStoredSpan right.evidenceStoredSpan

sameStoredSource :: TermMatchEvidence -> TermMatchEvidence -> Bool
sameStoredSource left right =
  left.evidenceField == right.evidenceField
    && left.evidenceNameVariant == right.evidenceNameVariant
    && left.evidenceSourceSequence == right.evidenceSourceSequence

spansOverlap :: TokenSpan -> TokenSpan -> Bool
spansOverlap left right =
  left.tokenSpanStart < spanEnd right
    && right.tokenSpanStart < spanEnd left

selectionMatchedPositionCount :: EvidenceSelection -> Int
selectionMatchedPositionCount selection =
  sum [item.evidenceQuerySpan.tokenSpanLength | item <- selection.selectionEvidence]

selectionExactCanonicalCount :: EvidenceSelection -> Int
selectionExactCanonicalCount selection =
  length
    [ ()
    | item <- selection.selectionEvidence,
      item.evidenceMatchKind `elem` [TokenMatchExact, TokenMatchCanonical]
    ]

selectionNameFieldCount :: EvidenceSelection -> Int
selectionNameFieldCount selection =
  length [() | item <- selection.selectionEvidence, item.evidenceField == SearchName]

selectionPhraseLength :: EvidenceSelection -> Int
selectionPhraseLength selection =
  sum [item.evidenceQuerySpan.tokenSpanLength | item <- selection.selectionEvidence, item.evidenceQuerySpan.tokenSpanLength > 1]

selectionEvidenceKey :: EvidenceSelection -> [(TokenSpan, SymbolSearchField, NE.NonEmpty SearchToken, TokenSpan, NE.NonEmpty SearchToken, Maybe NormalizedOccName)]
selectionEvidenceKey selection =
  [ (item.evidenceQuerySpan, item.evidenceField, item.evidenceStoredTokens, item.evidenceStoredSpan, item.evidenceSourceSequence, item.evidenceNameVariant)
  | item <- List.sortOn evidenceSortKey selection.selectionEvidence
  ]

evidenceSortKey :: TermMatchEvidence -> (TokenSpan, SymbolSearchField, NE.NonEmpty SearchToken, TokenSpan, NE.NonEmpty SearchToken, Maybe NormalizedOccName)
evidenceSortKey item =
  (item.evidenceQuerySpan, item.evidenceField, item.evidenceStoredTokens, item.evidenceStoredSpan, item.evidenceSourceSequence, item.evidenceNameVariant)

spanEnd :: TokenSpan -> Int
spanEnd span_ =
  span_.tokenSpanStart + span_.tokenSpanLength

orderedNameBonusFor :: SymbolSearchQuery -> IndexedNameVariant -> [TermMatchEvidence] -> Double
orderedNameBonusFor query nameVariant evidence
  | length query.symbolSearchTokens < 2 = 0
  | otherwise =
      variantBonus
  where
    nameEvidence =
      List.sortOn
        (.evidenceQuerySpan.tokenSpanStart)
        [ item
        | item <- evidence,
          item.evidenceNameVariant == Just nameVariant.indexedName
        ]
    variantBonus =
      let consumedQueryPositions = sum (map (.evidenceQuerySpan.tokenSpanLength) nameEvidence)
       in if consumedQueryPositions >= 2 && evidenceSpansInStoredOrder nameEvidence
            then orderedNameWeight * fromIntegral consumedQueryPositions / fromIntegral (length query.symbolSearchTokens)
            else 0

evidenceSpansInStoredOrder :: [TermMatchEvidence] -> Bool
evidenceSpansInStoredOrder evidence =
  and (zipWith orderedPair evidence (drop 1 evidence))
  where
    orderedPair left right =
      left.evidenceStoredSpan.tokenSpanStart < right.evidenceStoredSpan.tokenSpanStart
        && spanEnd left.evidenceStoredSpan <= right.evidenceStoredSpan.tokenSpanStart

nameSpecificityBonusFor :: IndexedNameVariant -> [TermMatchEvidence] -> Double
nameSpecificityBonusFor nameVariant evidence =
  if totalTokens == 0
    then 0
    else nameSpecificityWeight * fromIntegral (Set.size coveredPositions) / fromIntegral totalTokens
  where
    coveredPositions =
      Set.fromList
        [ position
        | item <- evidence,
          item.evidenceNameVariant == Just nameVariant.indexedName,
          position <- [item.evidenceStoredSpan.tokenSpanStart .. spanEnd item.evidenceStoredSpan - 1]
        ]
    totalTokens = length (NE.toList nameVariant.indexedNameTokens)

scoreBreakdownTotal :: SymbolScoreBreakdown -> Double
scoreBreakdownTotal breakdown =
  breakdown.matchedEvidenceScore
    - breakdown.unmatchedTokenPenalty
    + breakdown.orderedNameBonus
    + breakdown.nameSpecificityBonus
    - breakdown.capitalizationPenalty

tokenIdf :: SymbolSearchIndex -> SymbolSearchField -> SearchToken -> Double
tokenIdf index field token =
  logBase 2 ((fromIntegral totalDocuments + 1) / (fromIntegral tokenCount + 1)) + 1
  where
    tokenFrequency = Map.findWithDefault Map.empty field index.searchDocumentFrequencies
    tokenCount = Map.findWithDefault 0 token tokenFrequency
    totalDocuments = Map.findWithDefault 0 field index.searchFieldDocumentCounts

tokenIdfIfPresent :: SymbolSearchIndex -> SymbolSearchField -> SearchToken -> Maybe Double
tokenIdfIfPresent index field token =
  case Map.lookup token (Map.findWithDefault Map.empty field index.searchDocumentFrequencies) of
    Nothing -> Nothing
    Just _ -> Just (tokenIdf index field token)

matchTermIdf :: SymbolSearchIndex -> SymbolSearchField -> QueryTermMatch -> StoredTermOccurrence -> Double
matchTermIdf index field match occurrence =
  case match.matchedKind of
    TokenMatchExact ->
      singleStoredIdf
    TokenMatchCanonical ->
      singleStoredIdf
    TokenMatchSynonym ->
      synonymIdf
    TokenMatchFuzzy ->
      approximateSingleTokenIdf
  where
    singleStoredIdf = tokenIdf index field (NE.head occurrence.occurrenceStoredTokens)
    singleQueryIdf = maybe 1.0 id (tokenIdfIfPresent index field (NE.head match.matchedQueryTokens))
    approximateSingleTokenIdf = min singleStoredIdf singleQueryIdf
    storedTermIdf =
      minimum (map (tokenIdf index field) (NE.toList occurrence.occurrenceStoredTokens))
    queryTermIdfs =
      traverse (tokenIdfIfPresent index field) (NE.toList match.matchedQueryTokens)
    queryTermIdf =
      maybe 1.0 minimum queryTermIdfs
    synonymIdf =
      min storedTermIdf queryTermIdf

wholeNameDistance :: Text -> Text -> Int
wholeNameDistance query lookupName =
  EditDistance.restrictedDamerauLevenshteinDistance
    EditDistance.defaultEditCosts
    (T.unpack (T.toLower query))
    (T.unpack (T.toLower lookupName))

fieldWeight :: SymbolSearchField -> Double
fieldWeight = \case
  SearchName -> 1.0
  SearchResultType -> 0.65
  SearchArgumentType -> 0.65
  SearchModule -> 0.45

capitalizationMismatchPenalty :: Text -> Text -> Double
capitalizationMismatchPenalty query candidate =
  case (firstAlphabeticChar query, firstAlphabeticChar candidate) of
    (Just queryChar, Just candidateChar)
      | isUpper queryChar == isUpper candidateChar ->
          0
      | otherwise ->
          capitalizationMismatchPenaltyWeight
    _ ->
      0

firstAlphabeticChar :: Text -> Maybe Char
firstAlphabeticChar =
  T.find isLetter

unmatchedTokenPenaltyWeight :: Double
unmatchedTokenPenaltyWeight = 0.75

orderedNameWeight :: Double
orderedNameWeight = 0.5

nameSpecificityWeight :: Double
nameSpecificityWeight = 0.6

capitalizationMismatchPenaltyWeight :: Double
capitalizationMismatchPenaltyWeight = 0.6
