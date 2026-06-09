module Lore.Internal.Lookup.Search.Score
  ( buildSearchIndex,
    searchOccurrences,
  )
where

import Data.Char (isLetter, isUpper)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.Lookup.Search.Tokenize
  ( canonicalizeSearchToken,
    tokenSynonymRepresentative,
    tokenizeSearchText,
  )
import Lore.Internal.Lookup.Search.Types
  ( IndexedOccurrence (..),
    QueryTokenMatch (..),
    SearchContextField (..),
    SearchDocument (..),
    SearchResult (..),
    SearchToken (..),
    TokenMatchKind (..),
    TokenSearchIndex (..),
  )
import qualified Text.EditDistance as EditDistance

buildSearchIndex :: (Ord key) => [(key, SearchDocument, value)] -> TokenSearchIndex key value
buildSearchIndex occurrences =
  TokenSearchIndex
    { indexedOccurrences = Map.fromList indexedOccurrencePairs,
      occurrencesByToken,
      primaryTokenFrequency,
      contextTokenFrequency,
      totalPrimaryOccurrences,
      totalContextOccurrences
    }
  where
    indexedOccurrencePairs =
      [ (key, IndexedOccurrence key document.primaryText primaryTokens contextTokens value)
      | (key, document, value) <- occurrences,
        let primaryTokens = tokenizeSearchText document.primaryText,
        let contextTokens =
              Set.fromList . concatMap tokenizeSearchText
                <$> document.contextTexts
      ]

    occurrencesByToken =
      Map.fromListWith
        Set.union
        [ (token, Set.singleton key)
        | (key, IndexedOccurrence {indexedOccurrencePrimaryTokens}) <- indexedOccurrencePairs,
          token <- Set.toList (Set.fromList indexedOccurrencePrimaryTokens)
        ]

    primaryDocumentsByText =
      Map.fromList
        [ (occurrence.indexedOccurrencePrimaryText, occurrence.indexedOccurrencePrimaryTokens)
        | (_, occurrence) <- indexedOccurrencePairs
        ]

    primaryTokenFrequency =
      Map.fromListWith
        (+)
        [ (token, 1)
        | primaryTokens <- Map.elems primaryDocumentsByText,
          token <- Set.toList (Set.fromList primaryTokens)
        ]

    totalPrimaryOccurrences =
      Map.size primaryDocumentsByText

    contextTokenFrequency =
      Map.fromListWith
        (Map.unionWith (+))
        [ (field, Map.singleton token 1)
        | (_, occurrence) <- indexedOccurrencePairs,
          (field, tokens) <- Map.toList occurrence.indexedOccurrenceContextTokens,
          token <- Set.toList tokens
        ]

    totalContextOccurrences =
      Map.fromListWith
        (+)
        [ (field, 1)
        | (_, occurrence) <- indexedOccurrencePairs,
          (field, tokens) <- Map.toList occurrence.indexedOccurrenceContextTokens,
          not (Set.null tokens)
        ]

searchOccurrences :: (Ord key) => Text -> TokenSearchIndex key value -> [SearchResult key value]
searchOccurrences query index =
  List.sortOn resultSortKey $
    map (scoreOccurrence query loweredQuery queryTokens index queryTokenMatches) candidateOccurrences
  where
    loweredQuery = T.toLower query
    queryTokens = tokenizeSearchText query
    queryTokenMatches = matchQueryTokens index queryTokens
    candidateOccurrences =
      mapMaybe (`Map.lookup` index.indexedOccurrences) $
        Set.toList $
          Set.unions
            [ Map.findWithDefault Set.empty match.matchedToken index.occurrencesByToken
            | match <- queryTokenMatches
            ]

matchQueryTokens :: TokenSearchIndex key value -> [SearchToken] -> [QueryTokenMatch]
matchQueryTokens index queryTokens =
  concatMap matchQueryToken queryTokens
  where
    storedTokens =
      Set.toList $
        Map.keysSet index.primaryTokenFrequency
          <> foldMap Map.keysSet (Map.elems index.contextTokenFrequency)

    matchQueryToken queryToken =
      mapMaybe (mkTokenMatch queryToken) storedTokens

    mkTokenMatch queryToken storedToken =
      case classifyTokenMatch queryToken storedToken of
        Just (matchKind, matchDistance) ->
          Just
            QueryTokenMatch
              { queryToken,
                matchedToken = storedToken,
                tokenMatchKind = matchKind,
                tokenDistance = matchDistance,
                tokenSimilarityWeight =
                  tokenMatchSimilarity matchKind matchDistance
              }
        Nothing ->
          mkFuzzyTokenMatch
      where
        mkFuzzyTokenMatch =
          let distance = tokenEditDistance queryToken storedToken
              threshold = maxTokenDistance (T.length (unSearchToken queryToken))
           in if distance <= threshold
                then
                  Just
                    QueryTokenMatch
                      { queryToken,
                        matchedToken = storedToken,
                        tokenMatchKind = TokenMatchFuzzy,
                        tokenDistance = distance,
                        tokenSimilarityWeight =
                          tokenMatchSimilarity TokenMatchFuzzy distance
                      }
                else Nothing

scoreOccurrence ::
  Text ->
  Text ->
  [SearchToken] ->
  TokenSearchIndex key value ->
  [QueryTokenMatch] ->
  IndexedOccurrence key value ->
  SearchResult key value
scoreOccurrence query loweredQuery queryTokens index queryTokenMatches occurrence =
  SearchResult
    { searchResultKey = occurrence.indexedOccurrenceKey,
      searchResultText = occurrence.indexedOccurrencePrimaryText,
      searchResultValue = occurrence.indexedOccurrenceValue,
      searchResultScore = scoreBreakdownTotal breakdown,
      searchResultWholeDistance = wholeDistance
    }
  where
    breakdown =
      scoreOccurrenceBreakdown defaultSearchWeights query loweredQuery queryTokens index queryTokenMatches occurrence wholeDistance
    loweredOccurrenceText =
      T.toLower occurrence.indexedOccurrencePrimaryText
    wholeDistance =
      EditDistance.restrictedDamerauLevenshteinDistance
        EditDistance.defaultEditCosts
        (T.unpack loweredQuery)
        (T.unpack loweredOccurrenceText)

data SearchWeights = SearchWeights
  { exactTokenBonusWeight :: !Double,
    coverageBonusWeight :: !Double,
    fullCoverageNoExtraBonusWeight :: !Double,
    orderedTokenBonusWeight :: !Double,
    tokenDistancePenaltyWeight :: !Double,
    missingTokenPenaltyWeight :: !Double,
    extraTokenPenaltyWeight :: !Double,
    wholeDistancePenaltyWeight :: !Double,
    exactTextMatchBonusWeight :: !Double,
    capitalizationMismatchPenaltyWeight :: !Double,
    moduleTokenWeight :: !Double,
    resultTypeTokenWeight :: !Double,
    argumentTypeTokenWeight :: !Double
  }

defaultSearchWeights :: SearchWeights
defaultSearchWeights =
  SearchWeights
    { exactTokenBonusWeight = 0.25,
      coverageBonusWeight = 2.0,
      fullCoverageNoExtraBonusWeight = 0.6,
      orderedTokenBonusWeight = 0.5,
      tokenDistancePenaltyWeight = 0.35,
      missingTokenPenaltyWeight = 0.75,
      extraTokenPenaltyWeight = 0.15,
      wholeDistancePenaltyWeight = 0.02,
      exactTextMatchBonusWeight = 100.0,
      capitalizationMismatchPenaltyWeight = 0.6,
      moduleTokenWeight = 0.45,
      resultTypeTokenWeight = 0.65,
      argumentTypeTokenWeight = 0.65
    }

data SelectedTokenMatch = SelectedTokenMatch
  { selectedQueryMatch :: QueryTokenMatch,
    selectedMatchField :: !SearchField,
    selectedMatchIdf :: !Double,
    selectedMatchStrength :: !Double
  }

data SearchField
  = SearchFieldPrimary
  | SearchFieldContext SearchContextField
  deriving stock (Eq, Ord)

data ScoreBreakdown = ScoreBreakdown
  { scoreBreakdownMatchedTokenScore :: !Double,
    scoreBreakdownExactTokenBonus :: !Double,
    scoreBreakdownCoverageBonus :: !Double,
    scoreBreakdownFullCoverageNoExtraBonus :: !Double,
    scoreBreakdownOrderedBonus :: !Double,
    scoreBreakdownExactTextMatchBonus :: !Double,
    scoreBreakdownTokenDistancePenalty :: !Double,
    scoreBreakdownMissingImportantTokenPenalty :: !Double,
    scoreBreakdownExtraTokenPenalty :: !Double,
    scoreBreakdownWholeDistancePenalty :: !Double,
    scoreBreakdownCapitalizedCandidatePenalty :: !Double
  }

scoreBreakdownTotal :: ScoreBreakdown -> Double
scoreBreakdownTotal breakdown =
  breakdown.scoreBreakdownMatchedTokenScore
    + breakdown.scoreBreakdownExactTokenBonus
    + breakdown.scoreBreakdownCoverageBonus
    + breakdown.scoreBreakdownFullCoverageNoExtraBonus
    + breakdown.scoreBreakdownOrderedBonus
    + breakdown.scoreBreakdownExactTextMatchBonus
    - breakdown.scoreBreakdownTokenDistancePenalty
    - breakdown.scoreBreakdownMissingImportantTokenPenalty
    - breakdown.scoreBreakdownExtraTokenPenalty
    - breakdown.scoreBreakdownWholeDistancePenalty
    - breakdown.scoreBreakdownCapitalizedCandidatePenalty

scoreOccurrenceBreakdown ::
  SearchWeights ->
  Text ->
  Text ->
  [SearchToken] ->
  TokenSearchIndex key value ->
  [QueryTokenMatch] ->
  IndexedOccurrence key value ->
  Int ->
  ScoreBreakdown
scoreOccurrenceBreakdown weights query loweredQuery queryTokens index queryTokenMatches occurrence wholeDistance =
  ScoreBreakdown
    { scoreBreakdownMatchedTokenScore = matchedTokenScore,
      scoreBreakdownExactTokenBonus = exactTokenBonus,
      scoreBreakdownCoverageBonus = coverageBonus,
      scoreBreakdownFullCoverageNoExtraBonus = fullCoverageNoExtraBonus,
      scoreBreakdownOrderedBonus = orderedBonus,
      scoreBreakdownExactTextMatchBonus = exactTextMatchBonus,
      scoreBreakdownTokenDistancePenalty = tokenDistancePenalty,
      scoreBreakdownMissingImportantTokenPenalty = missingImportantTokenPenalty,
      scoreBreakdownExtraTokenPenalty = extraTokenPenalty,
      scoreBreakdownWholeDistancePenalty = wholeDistancePenalty,
      scoreBreakdownCapitalizedCandidatePenalty = capitalizedCandidatePenalty
    }
  where
    primaryTokenSet = Set.fromList occurrence.indexedOccurrencePrimaryTokens
    bestMatches =
      [ bestMatch
      | queryToken <- queryTokens,
        Just bestMatch <- [bestCandidateMatch weights index primaryTokenSet occurrence.indexedOccurrenceContextTokens queryTokenMatches queryToken]
      ]
    coverageByQueryToken =
      Map.fromListWith
        max
        [ (selectedMatch.selectedQueryMatch.queryToken, selectedMatch.selectedMatchStrength)
        | selectedMatch <- bestMatches
        ]
    matchedCandidateTokens =
      Set.fromList (map (.selectedQueryMatch.matchedToken) (filter isPrimarySelectedMatch bestMatches))
    extraTokenCount =
      Set.size (primaryTokenSet `Set.difference` matchedCandidateTokens)
    matchedTokenScore =
      sum (map selectedWeightedTokenScore bestMatches)
    exactTokenBonus =
      weights.exactTokenBonusWeight
        * sum
          [ selectedMatch.selectedMatchStrength
          | selectedMatch <- bestMatches,
            selectedMatch.selectedQueryMatch.tokenMatchKind == TokenMatchExact
          ]
    coverageBonus =
      if null queryTokens
        then 0
        else weights.coverageBonusWeight * sum (Map.elems coverageByQueryToken) / fromIntegral (length queryTokens)
    fullCoverageNoExtraBonus =
      if Map.size coverageByQueryToken == length queryTokens && all (>= 1.0) (Map.elems coverageByQueryToken) && extraTokenCount == 0
        then weights.fullCoverageNoExtraBonusWeight
        else 0
    orderedBonus =
      if tokensMatchInOrder occurrence.indexedOccurrencePrimaryTokens (map (.selectedQueryMatch.matchedToken) (filter isPrimarySelectedMatch bestMatches))
        then weights.orderedTokenBonusWeight * fromIntegral (length (filter isPrimarySelectedMatch bestMatches))
        else 0
    tokenDistancePenalty =
      weights.tokenDistancePenaltyWeight
        * sum
          [ fromIntegral selectedMatch.selectedQueryMatch.tokenDistance * selectedMatch.selectedMatchStrength
          | selectedMatch <- bestMatches
          ]
    missingImportantTokenPenalty =
      sum [missingTokenPenalty weights index queryToken * missingTokenStrength queryToken | queryToken <- queryTokens]
    extraTokenPenalty =
      weights.extraTokenPenaltyWeight * fromIntegral extraTokenCount
    loweredOccurrenceText =
      T.toLower occurrence.indexedOccurrencePrimaryText
    wholeDistancePenalty =
      weights.wholeDistancePenaltyWeight * fromIntegral wholeDistance
    exactTextMatchBonus =
      if loweredQuery == loweredOccurrenceText
        then weights.exactTextMatchBonusWeight
        else 0
    capitalizedCandidatePenalty =
      capitalizationMismatchPenalty weights query occurrence.indexedOccurrencePrimaryText
    missingTokenStrength queryToken =
      1.0 - Map.findWithDefault 0 queryToken coverageByQueryToken

capitalizationMismatchPenalty :: SearchWeights -> Text -> Text -> Double
capitalizationMismatchPenalty weights query candidate =
  case (firstAlphabeticChar query, firstAlphabeticChar candidate) of
    (Just queryChar, Just candidateChar)
      | isUpper queryChar == isUpper candidateChar ->
          0
      | otherwise ->
          weights.capitalizationMismatchPenaltyWeight
    _ ->
      0

firstAlphabeticChar :: Text -> Maybe Char
firstAlphabeticChar text =
  T.find isLetter text

bestCandidateMatch :: SearchWeights -> TokenSearchIndex key value -> Set.Set SearchToken -> Map.Map SearchContextField (Set.Set SearchToken) -> [QueryTokenMatch] -> SearchToken -> Maybe SelectedTokenMatch
bestCandidateMatch weights index primaryTokens contextTokens queryTokenMatches queryToken =
  case matches of
    [] -> Nothing
    _ -> Just $ List.maximumBy (compare `on` selectedTokenMatchSortKey) matches
  where
    matches =
      [ SelectedTokenMatch match field (matchTokenIdf index field match.queryToken match.matchedToken match.tokenMatchKind) strength
      | match <- queryTokenMatches,
        match.queryToken == queryToken,
        (field, strength) <- tokenMatchFieldStrength match.matchedToken
      ]
    tokenMatchFieldStrength matchedToken
      | matchedToken `Set.member` primaryTokens = [(SearchFieldPrimary, primaryTokenWeight)]
      | otherwise =
          [ (SearchFieldContext field, contextFieldWeight weights field)
          | (field, tokens) <- Map.toList contextTokens,
            matchedToken `Set.member` tokens
          ]

selectedTokenMatchSortKey :: SelectedTokenMatch -> (Double, Double, Down Int, Down Int, SearchToken)
selectedTokenMatchSortKey selectedMatch =
  ( selectedWeightedTokenScore selectedMatch,
    selectedMatch.selectedMatchStrength,
    Down (tokenMatchKindPriority selectedMatch.selectedQueryMatch.tokenMatchKind),
    Down selectedMatch.selectedQueryMatch.tokenDistance,
    selectedMatch.selectedQueryMatch.matchedToken
  )

selectedWeightedTokenScore :: SelectedTokenMatch -> Double
selectedWeightedTokenScore selectedMatch =
  selectedMatch.selectedQueryMatch.tokenSimilarityWeight
    * selectedMatch.selectedMatchIdf
    * selectedMatch.selectedMatchStrength

isPrimarySelectedMatch :: SelectedTokenMatch -> Bool
isPrimarySelectedMatch selectedMatch =
  selectedMatch.selectedMatchField == SearchFieldPrimary

tokensMatchInOrder :: [SearchToken] -> [SearchToken] -> Bool
tokensMatchInOrder candidateTokens matchedTokens =
  go candidateTokens matchedTokens
  where
    go _ [] = True
    go [] _ = False
    go (candidateToken : remainingCandidates) tokens@(matchedToken : remainingMatched)
      | candidateToken == matchedToken = go remainingCandidates remainingMatched
      | otherwise = go remainingCandidates tokens

tokenIdf :: SearchField -> TokenSearchIndex key value -> SearchToken -> Double
tokenIdf field index token =
  logBase 2 ((fromIntegral totalOccurrences + 1) / (fromIntegral tokenCount + 1)) + 1
  where
    (tokenFrequency, totalOccurrences) = tokenFrequencyStats field index
    tokenCount =
      Map.findWithDefault 0 token tokenFrequency

tokenIdfIfPresent :: SearchField -> TokenSearchIndex key value -> SearchToken -> Maybe Double
tokenIdfIfPresent field index token =
  case Map.lookup token tokenFrequency of
    Nothing -> Nothing
    Just tokenCount ->
      Just $
        logBase 2 ((fromIntegral totalOccurrences + 1) / (fromIntegral tokenCount + 1))
          + 1
  where
    (tokenFrequency, totalOccurrences) = tokenFrequencyStats field index

matchTokenIdf :: TokenSearchIndex key value -> SearchField -> SearchToken -> SearchToken -> TokenMatchKind -> Double
matchTokenIdf index field queryToken storedToken matchKind =
  case matchKind of
    TokenMatchExact ->
      storedTokenIdf
    TokenMatchCanonical ->
      approximateMatchIdf
    TokenMatchSynonym ->
      approximateMatchIdf
    TokenMatchFuzzy ->
      approximateMatchIdf
  where
    storedTokenIdf =
      tokenIdf field index storedToken

    approximateMatchIdf =
      maybe storedTokenIdf (min storedTokenIdf) (tokenIdfIfPresent field index queryToken)

missingTokenPenalty :: SearchWeights -> TokenSearchIndex key value -> SearchToken -> Double
missingTokenPenalty weights index token =
  weights.missingTokenPenaltyWeight * tokenIdf SearchFieldPrimary index token

tokenFrequencyStats :: SearchField -> TokenSearchIndex key value -> (Map.Map SearchToken Int, Int)
tokenFrequencyStats field index =
  case field of
    SearchFieldPrimary ->
      (index.primaryTokenFrequency, index.totalPrimaryOccurrences)
    SearchFieldContext contextField ->
      ( Map.findWithDefault Map.empty contextField index.contextTokenFrequency,
        Map.findWithDefault 0 contextField index.totalContextOccurrences
      )

primaryTokenWeight :: Double
primaryTokenWeight =
  1.0

contextFieldWeight :: SearchWeights -> SearchContextField -> Double
contextFieldWeight weights = \case
  SearchContextModule ->
    weights.moduleTokenWeight
  SearchContextResultType ->
    weights.resultTypeTokenWeight
  SearchContextArgumentType ->
    weights.argumentTypeTokenWeight

tokenSimilarity :: Int -> Double
tokenSimilarity distance =
  1 / (1 + fromIntegral distance)

tokenMatchSimilarity :: TokenMatchKind -> Int -> Double
tokenMatchSimilarity matchKind distance =
  case matchKind of
    TokenMatchExact ->
      1.0
    TokenMatchCanonical ->
      canonicalMatchSimilarity
    TokenMatchSynonym ->
      synonymMatchSimilarity
    TokenMatchFuzzy ->
      tokenSimilarity distance

tokenMatchKindPriority :: TokenMatchKind -> Int
tokenMatchKindPriority matchKind =
  case matchKind of
    TokenMatchExact -> 4
    TokenMatchCanonical -> 3
    TokenMatchSynonym -> 2
    TokenMatchFuzzy -> 1

classifyTokenMatch :: SearchToken -> SearchToken -> Maybe (TokenMatchKind, Int)
classifyTokenMatch queryToken storedToken
  | queryToken == storedToken =
      Just (TokenMatchExact, 0)
  | canonicalQueryToken == canonicalStoredToken =
      Just (TokenMatchCanonical, 0)
  | shareSynonymRepresentative canonicalQueryToken canonicalStoredToken =
      Just (TokenMatchSynonym, 1)
  | otherwise =
      Nothing
  where
    canonicalQueryToken = canonicalizeSearchToken queryToken
    canonicalStoredToken = canonicalizeSearchToken storedToken

shareSynonymRepresentative :: SearchToken -> SearchToken -> Bool
shareSynonymRepresentative leftToken rightToken =
  case (tokenSynonymRepresentative leftToken, tokenSynonymRepresentative rightToken) of
    (Just leftRep, Just rightRep) -> leftRep == rightRep
    _ -> False

canonicalMatchSimilarity :: Double
canonicalMatchSimilarity =
  0.95

synonymMatchSimilarity :: Double
synonymMatchSimilarity =
  0.85

tokenEditDistance :: SearchToken -> SearchToken -> Int
tokenEditDistance queryToken storedToken =
  EditDistance.restrictedDamerauLevenshteinDistance
    EditDistance.defaultEditCosts
    (T.unpack (unSearchToken queryToken))
    (T.unpack (unSearchToken storedToken))

maxTokenDistance :: Int -> Int
maxTokenDistance tokenLength
  | tokenLength <= 2 = 0
  | tokenLength <= 4 = 1
  | tokenLength <= 6 = 2
  | tokenLength <= 12 = 3
  | otherwise = 4

resultSortKey :: SearchResult key value -> (Down Double, Int, Text)
resultSortKey result =
  (Down result.searchResultScore, result.searchResultWholeDistance, result.searchResultText)
