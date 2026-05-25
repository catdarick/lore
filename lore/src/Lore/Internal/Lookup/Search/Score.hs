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
    SearchResult (..),
    SearchToken (..),
    TokenMatchKind (..),
    TokenSearchIndex (..),
  )
import qualified Text.EditDistance as EditDistance

buildSearchIndex :: (Ord key) => [(key, Text, value)] -> TokenSearchIndex key value
buildSearchIndex occurrences =
  TokenSearchIndex
    { indexedOccurrences = Map.fromList indexedOccurrencePairs,
      occurrencesByToken,
      tokenFrequency,
      totalOccurrences = length indexedOccurrencePairs
    }
  where
    indexedOccurrencePairs =
      [ (key, IndexedOccurrence key text (tokenizeSearchText text) value)
      | (key, text, value) <- occurrences
      ]

    occurrencesByToken =
      Map.fromListWith
        Set.union
        [ (token, Set.singleton key)
        | (key, IndexedOccurrence {indexedOccurrenceTokens}) <- indexedOccurrencePairs,
          token <- Set.toList (Set.fromList indexedOccurrenceTokens)
        ]

    tokenFrequency =
      length <$> occurrencesByToken

searchOccurrences :: (Ord key) => Int -> Text -> TokenSearchIndex key value -> [SearchResult key value]
searchOccurrences resultLimit query index =
  take resultLimit $
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
    storedTokens = Map.keys index.occurrencesByToken

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
                tokenWeight =
                  matchTokenIdf index queryToken storedToken matchKind
                    * tokenMatchSimilarity matchKind matchDistance
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
                        tokenWeight =
                          matchTokenIdf index queryToken storedToken TokenMatchFuzzy
                            * tokenMatchSimilarity TokenMatchFuzzy distance
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
      searchResultText = occurrence.indexedOccurrenceText,
      searchResultValue = occurrence.indexedOccurrenceValue,
      searchResultScore = score,
      searchResultWholeDistance = wholeDistance
    }
  where
    candidateTokenSet = Set.fromList occurrence.indexedOccurrenceTokens
    bestMatches =
      [ bestMatch
      | queryToken <- queryTokens,
        Just bestMatch <- [bestCandidateMatch candidateTokenSet queryTokenMatches queryToken]
      ]
    matchedQueryTokens =
      Set.fromList (map (.queryToken) bestMatches)
    missingQueryTokens =
      filter (`Set.notMember` matchedQueryTokens) queryTokens
    matchedCandidateTokens =
      Set.fromList (map (.matchedToken) bestMatches)
    extraTokenCount =
      Set.size (candidateTokenSet `Set.difference` matchedCandidateTokens)
    matchedTokenScore =
      sum (map (.tokenWeight) bestMatches)
    exactTokenBonus =
      0.25 * fromIntegral (length (filter ((== TokenMatchExact) . (.tokenMatchKind)) bestMatches))
    coverageBonus =
      if null queryTokens
        then 0
        else 2.0 * fromIntegral (Set.size matchedQueryTokens) / fromIntegral (length queryTokens)
    fullCoverageNoExtraBonus =
      if Set.size matchedQueryTokens == length queryTokens && extraTokenCount == 0
        then fullCoverageNoExtraBonusWeight
        else 0
    orderedBonus =
      if tokensMatchInOrder occurrence.indexedOccurrenceTokens (map (.matchedToken) bestMatches)
        then 0.5 * fromIntegral (length bestMatches)
        else 0
    tokenDistancePenalty =
      0.35 * fromIntegral (sum (map (.tokenDistance) bestMatches))
    missingImportantTokenPenalty =
      sum (map (missingTokenPenalty index) missingQueryTokens)
    extraTokenPenalty =
      0.15 * fromIntegral extraTokenCount
    loweredOccurrenceText =
      T.toLower occurrence.indexedOccurrenceText
    wholeDistance =
      EditDistance.restrictedDamerauLevenshteinDistance
        EditDistance.defaultEditCosts
        (T.unpack loweredQuery)
        (T.unpack loweredOccurrenceText)
    wholeDistancePenalty =
      0.02 * fromIntegral wholeDistance
    exactTextMatchBonus =
      if loweredQuery == loweredOccurrenceText
        then exactTextMatchBonusWeight
        else 0
    capitalizedCandidatePenalty =
      capitalizationMismatchPenalty query occurrence.indexedOccurrenceText
    score =
      matchedTokenScore
        + exactTokenBonus
        + coverageBonus
        + fullCoverageNoExtraBonus
        + orderedBonus
        + exactTextMatchBonus
        - tokenDistancePenalty
        - missingImportantTokenPenalty
        - extraTokenPenalty
        - wholeDistancePenalty
        - capitalizedCandidatePenalty

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

capitalizationMismatchPenaltyWeight :: Double
capitalizationMismatchPenaltyWeight =
  0.6

firstAlphabeticChar :: Text -> Maybe Char
firstAlphabeticChar text =
  T.find isLetter text

bestCandidateMatch :: Set.Set SearchToken -> [QueryTokenMatch] -> SearchToken -> Maybe QueryTokenMatch
bestCandidateMatch candidateTokens queryTokenMatches queryToken =
  case matches of
    [] -> Nothing
    _ -> Just $ List.maximumBy (compare `on` tokenMatchSortKey) matches
  where
    matches =
      [ match
      | match <- queryTokenMatches,
        match.queryToken == queryToken,
        match.matchedToken `Set.member` candidateTokens
      ]

tokenMatchSortKey :: QueryTokenMatch -> (Double, Down Int, Down Int, SearchToken)
tokenMatchSortKey match =
  (match.tokenWeight, Down (tokenMatchKindPriority match.tokenMatchKind), Down match.tokenDistance, match.matchedToken)

tokensMatchInOrder :: [SearchToken] -> [SearchToken] -> Bool
tokensMatchInOrder candidateTokens matchedTokens =
  go candidateTokens matchedTokens
  where
    go _ [] = True
    go [] _ = False
    go (candidateToken : remainingCandidates) tokens@(matchedToken : remainingMatched)
      | candidateToken == matchedToken = go remainingCandidates remainingMatched
      | otherwise = go remainingCandidates tokens

tokenIdf :: TokenSearchIndex key value -> SearchToken -> Double
tokenIdf index token =
  logBase 2 ((fromIntegral index.totalOccurrences + 1) / (fromIntegral tokenCount + 1)) + 1
  where
    tokenCount =
      Map.findWithDefault 0 token index.tokenFrequency

tokenIdfIfPresent :: TokenSearchIndex key value -> SearchToken -> Maybe Double
tokenIdfIfPresent index token =
  case Map.lookup token index.tokenFrequency of
    Nothing -> Nothing
    Just tokenCount ->
      Just $
        logBase 2 ((fromIntegral index.totalOccurrences + 1) / (fromIntegral tokenCount + 1))
          + 1

matchTokenIdf :: TokenSearchIndex key value -> SearchToken -> SearchToken -> TokenMatchKind -> Double
matchTokenIdf index queryToken storedToken matchKind =
  case matchKind of
    TokenMatchSynonym ->
      let storedTokenIdf = tokenIdf index storedToken
       in maybe storedTokenIdf (`min` storedTokenIdf) (tokenIdfIfPresent index queryToken)
    _ ->
      tokenIdf index storedToken

missingTokenPenalty :: TokenSearchIndex key value -> SearchToken -> Double
missingTokenPenalty index token =
  0.75 * tokenIdf index token

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

exactTextMatchBonusWeight :: Double
exactTextMatchBonusWeight =
  100.0

fullCoverageNoExtraBonusWeight :: Double
fullCoverageNoExtraBonusWeight =
  0.6

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
