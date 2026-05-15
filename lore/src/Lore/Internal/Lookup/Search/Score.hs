module Lore.Internal.Lookup.Search.Score
  ( buildSearchIndex,
    searchOccurrences,
    rankSearchTexts,
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
import Lore.Internal.Lookup.Search.Tokenize (tokenizeSearchText)
import Lore.Internal.Lookup.Search.Types
  ( IndexedOccurrence (..),
    QueryTokenMatch (..),
    SearchResult (..),
    SearchToken (..),
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
      map (scoreOccurrence query index queryTokenMatches) candidateOccurrences
  where
    queryTokens = tokenizeSearchText query
    queryTokenMatches = matchQueryTokens index queryTokens
    candidateOccurrences =
      mapMaybe (`Map.lookup` index.indexedOccurrences) $
        Set.toList $
          Set.unions
            [ Map.findWithDefault Set.empty match.matchedToken index.occurrencesByToken
            | match <- queryTokenMatches
            ]

rankSearchTexts :: Int -> Text -> [Text] -> [Text]
rankSearchTexts resultLimit query candidates =
  map (.searchResultText) $
    searchOccurrences resultLimit query $
      buildSearchIndex [(candidate, candidate, ()) | candidate <- candidates]

matchQueryTokens :: TokenSearchIndex key value -> [SearchToken] -> [QueryTokenMatch]
matchQueryTokens index queryTokens =
  concatMap matchQueryToken queryTokens
  where
    storedTokens = Map.keys index.occurrencesByToken

    matchQueryToken queryToken =
      mapMaybe (mkTokenMatch queryToken) storedTokens

    mkTokenMatch queryToken storedToken =
      let distance = tokenEditDistance queryToken storedToken
          threshold = maxTokenDistance (T.length (unSearchToken queryToken))
       in if distance <= threshold
            then
              Just
                QueryTokenMatch
                  { queryToken,
                    matchedToken = storedToken,
                    tokenDistance = distance,
                    tokenWeight = tokenIdf index storedToken * tokenSimilarity distance
                  }
            else Nothing

scoreOccurrence :: Text -> TokenSearchIndex key value -> [QueryTokenMatch] -> IndexedOccurrence key value -> SearchResult key value
scoreOccurrence query index queryTokenMatches occurrence =
  SearchResult
    { searchResultKey = occurrence.indexedOccurrenceKey,
      searchResultText = occurrence.indexedOccurrenceText,
      searchResultValue = occurrence.indexedOccurrenceValue,
      searchResultScore = score,
      searchResultWholeDistance = wholeDistance
    }
  where
    candidateTokenSet = Set.fromList occurrence.indexedOccurrenceTokens
    queryTokens = tokenizeSearchText query
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
      0.25 * fromIntegral (length (filter ((== 0) . (.tokenDistance)) bestMatches))
    coverageBonus =
      if null queryTokens
        then 0
        else 2.0 * fromIntegral (Set.size matchedQueryTokens) / fromIntegral (length queryTokens)
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
    wholeDistance =
      EditDistance.restrictedDamerauLevenshteinDistance
        EditDistance.defaultEditCosts
        (T.unpack (T.toLower query))
        (T.unpack (T.toLower occurrence.indexedOccurrenceText))
    wholeDistancePenalty =
      0.02 * fromIntegral wholeDistance
    capitalizedCandidatePenalty =
      capitalizationMismatchPenalty query occurrence.indexedOccurrenceText
    score =
      matchedTokenScore
        + exactTokenBonus
        + coverageBonus
        + orderedBonus
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

tokenMatchSortKey :: QueryTokenMatch -> (Double, Down Int, SearchToken)
tokenMatchSortKey match =
  (match.tokenWeight, Down match.tokenDistance, match.matchedToken)

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

missingTokenPenalty :: TokenSearchIndex key value -> SearchToken -> Double
missingTokenPenalty index token =
  0.75 * tokenIdf index token

tokenSimilarity :: Int -> Double
tokenSimilarity distance =
  1 / (1 + fromIntegral distance)

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
