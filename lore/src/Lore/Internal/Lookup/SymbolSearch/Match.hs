module Lore.Internal.Lookup.SymbolSearch.Match
  ( matchQueryTerms,
    classifySingleTokenMatch,
    candidateNamesFromMatches,
    candidatesForStoredPattern,
    findContiguousTermOccurrences,
    queryTokenIsFirstOccurrence,
    tokenEditDistance,
    maxTokenDistance,
    tokenMatchQuality,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC.Types.Name as GHC
import Lore.Internal.Lookup.SymbolSearch.Synonyms (SynonymLexicon, SynonymTerm (..), directSynonyms)
import Lore.Internal.Lookup.SymbolSearch.Tokenize (canonicalizeSearchToken)
import Lore.Internal.Lookup.SymbolSearch.Types
  ( QueryTermMatch (..),
    SearchToken (..),
    StoredMatchPattern (..),
    SymbolSearchIndex (..),
    TokenMatchKind (..),
    TokenSpan (..),
  )
import qualified Text.EditDistance as EditDistance

matchQueryTerms :: SynonymLexicon -> SymbolSearchIndex -> [SearchToken] -> [QueryTermMatch]
matchQueryTerms lexicon index queryTokens =
  ordinaryMatches <> synonymMatches
  where
    storedTokens = Set.toList index.searchVocabulary
    indexedQueryTokens = zip [0 ..] queryTokens
    ordinaryMatches =
      [ match
      | (position, queryToken) <- indexedQueryTokens,
        queryTokenIsFirstOccurrence queryTokens position,
        storedToken <- storedTokens,
        match <- maybeToList (mkSingleTokenMatch position queryToken storedToken)
      ]
    synonymMatches =
      [ QueryTermMatch
          { matchedQuerySpan = span_,
            matchedQueryTokens = tokens,
            matchedStoredPattern = StoredSynonymTermPattern targetTerm,
            matchedKind = TokenMatchSynonym,
            matchedEditDistance = Nothing,
            matchedQuality = tokenMatchQuality TokenMatchSynonym Nothing
          }
      | (span_, tokens) <- querySpans queryTokens,
        span_.tokenSpanLength > 1 || queryTokenIsFirstOccurrence queryTokens span_.tokenSpanStart,
        let queryTerm = SynonymTerm (fmap canonicalizeSearchToken tokens),
        targetTerm <- Set.toList (directSynonyms lexicon queryTerm)
      ]

    mkSingleTokenMatch position queryToken storedToken =
      case classifySingleTokenMatch queryToken storedToken of
        Just (matchKind, matchDistance) ->
          Just
            QueryTermMatch
              { matchedQuerySpan = TokenSpan position 1,
                matchedQueryTokens = queryToken NE.:| [],
                matchedStoredPattern = StoredTokenPattern storedToken,
                matchedKind = matchKind,
                matchedEditDistance = matchDistance,
                matchedQuality = tokenMatchQuality matchKind matchDistance
              }
        Nothing ->
          let distance = tokenEditDistance queryToken storedToken
              threshold = maxTokenDistance (T.length queryToken.unSearchToken)
           in if distance <= threshold
                then
                  Just
                    QueryTermMatch
                      { matchedQuerySpan = TokenSpan position 1,
                        matchedQueryTokens = queryToken NE.:| [],
                        matchedStoredPattern = StoredTokenPattern storedToken,
                        matchedKind = TokenMatchFuzzy,
                        matchedEditDistance = Just distance,
                        matchedQuality = tokenMatchQuality TokenMatchFuzzy (Just distance)
                      }
                else Nothing

classifySingleTokenMatch :: SearchToken -> SearchToken -> Maybe (TokenMatchKind, Maybe Int)
classifySingleTokenMatch queryToken storedToken
  | queryToken == storedToken =
      Just (TokenMatchExact, Nothing)
  | canonicalQueryToken == canonicalStoredToken =
      Just (TokenMatchCanonical, Nothing)
  | otherwise =
      Nothing
  where
    canonicalQueryToken = canonicalizeSearchToken queryToken
    canonicalStoredToken = canonicalizeSearchToken storedToken

candidateNamesFromMatches :: SymbolSearchIndex -> [QueryTermMatch] -> Set.Set GHC.Name
candidateNamesFromMatches index matches =
  Set.unions
    [ candidatesForStoredPattern index match.matchedStoredPattern
    | match <- matches
    ]

candidatesForStoredPattern :: SymbolSearchIndex -> StoredMatchPattern -> Set.Set GHC.Name
candidatesForStoredPattern index pattern_ =
  Set.unions
    [ candidatesForField field fieldPostings
    | (field, fieldPostings) <- Map.toList index.searchPostings
    ]
  where
    candidatesForField _ fieldPostings =
      case pattern_ of
        StoredTokenPattern token ->
          Map.findWithDefault Set.empty token fieldPostings
        StoredSynonymTermPattern term ->
          candidateIntersectionForTerm index.searchTokensByCanonical fieldPostings term

candidateIntersectionForTerm :: Map.Map SearchToken (Set.Set SearchToken) -> Map.Map SearchToken (Set.Set GHC.Name) -> SynonymTerm -> Set.Set GHC.Name
candidateIntersectionForTerm tokensByCanonical fieldPostings (SynonymTerm termTokens) =
  case map postingsForCanonicalToken (NE.toList termTokens) of
    [] -> Set.empty
    firstPostings : restPostings ->
      foldl Set.intersection firstPostings restPostings
  where
    postingsForCanonicalToken canonicalToken =
      Set.unions
        [ Map.findWithDefault Set.empty rawToken fieldPostings
        | rawToken <- Set.toList (Map.findWithDefault Set.empty canonicalToken tokensByCanonical)
        ]

findContiguousTermOccurrences :: NonEmpty SearchToken -> NonEmpty SearchToken -> [TokenSpan]
findContiguousTermOccurrences storedTokens targetTerm =
  [ TokenSpan start targetLength
  | (start, window) <- windows targetLength (NE.toList storedTokens),
    map canonicalizeSearchToken window == canonicalTarget
  ]
  where
    canonicalTarget = map canonicalizeSearchToken (NE.toList targetTerm)
    targetLength = length canonicalTarget

queryTokenIsFirstOccurrence :: [SearchToken] -> Int -> Bool
queryTokenIsFirstOccurrence tokens position =
  case drop position tokens of
    [] -> False
    token : _ -> canonicalizeSearchToken token `notElem` map canonicalizeSearchToken (take position tokens)

querySpans :: [SearchToken] -> [(TokenSpan, NonEmpty SearchToken)]
querySpans queryTokens =
  [ (TokenSpan start len, tokens)
  | start <- [0 .. length queryTokens - 1],
    len <- [1 .. length queryTokens - start],
    Just tokens <- [NE.nonEmpty (take len (drop start queryTokens))]
  ]

windows :: Int -> [a] -> [(Int, [a])]
windows width values =
  [ (start, take width rest)
  | (start, rest) <- zip [0 ..] (tails values),
    length rest >= width
  ]

tails :: [a] -> [[a]]
tails [] = []
tails values@(_ : rest) = values : tails rest

tokenEditDistance :: SearchToken -> SearchToken -> Int
tokenEditDistance queryToken storedToken =
  EditDistance.restrictedDamerauLevenshteinDistance
    EditDistance.defaultEditCosts
    (T.unpack queryToken.unSearchToken)
    (T.unpack storedToken.unSearchToken)

maxTokenDistance :: Int -> Int
maxTokenDistance tokenLength
  | tokenLength <= 2 = 0
  | tokenLength <= 4 = 1
  | tokenLength <= 6 = 2
  | tokenLength <= 12 = 3
  | otherwise = 4

tokenMatchQuality :: TokenMatchKind -> Maybe Int -> Double
tokenMatchQuality matchKind distance =
  case matchKind of
    TokenMatchExact -> 1.0
    TokenMatchCanonical -> 0.95
    TokenMatchSynonym -> 0.85
    TokenMatchFuzzy -> maybe 0.0 (\value -> 1 / (1 + fromIntegral value)) distance

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]
