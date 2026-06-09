module Lore.Internal.Lookup.SymbolSearch.Rank
  ( parseSymbolSearchQuery,
    findSymbolSearchSuggestions,
    matchQueryTokens,
    classifyTokenMatch,
    tokenIdf,
    scoreSymbolDocument,
    wholeNameDistance,
  )
where

import Data.Char (isLetter, isUpper)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Types.Name as GHC
import Lore.Internal.Lookup.ModulePattern (ModulePattern, matchesModulePattern)
import Lore.Internal.Lookup.Name (NormalizedOccName, parseQualifiedNormalizedOccName, unNormalizedOccName)
import Lore.Internal.Lookup.SymbolSearch.Index (fieldTokens)
import Lore.Internal.Lookup.SymbolSearch.Synonyms (SynonymLexicon, areDirectSynonyms)
import Lore.Internal.Lookup.SymbolSearch.Tokenize (canonicalizeSearchToken, tokenizeSearchText)
import Lore.Internal.Lookup.SymbolSearch.Types
  ( IndexedNameVariant (..),
    QueryTokenMatch (..),
    SearchToken (..),
    SymbolScoreBreakdown (..),
    SymbolSearchDocument (..),
    SymbolSearchField (..),
    SymbolSearchIndex (..),
    SymbolSearchQuery (..),
    TokenMatchEvidence (..),
    TokenMatchKind (..),
  )
import Lore.Internal.Lookup.Types (Symbol (name), SymbolSuggestion (..))
import qualified Text.EditDistance as EditDistance

parseSymbolSearchQuery :: Text -> SymbolSearchQuery
parseSymbolSearchQuery rawQuery =
  SymbolSearchQuery
    { symbolSearchText = occName.unNormalizedOccName,
      symbolSearchTokens = dedupePreservingOrder (tokenizeSearchText occName.unNormalizedOccName),
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
    tokenMatches = matchQueryTokens lexicon index query.symbolSearchTokens
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

matchQueryTokens :: SynonymLexicon -> SymbolSearchIndex -> [SearchToken] -> [QueryTokenMatch]
matchQueryTokens lexicon index queryTokens =
  concatMap matchQueryToken queryTokens
  where
    storedTokens = Set.toList index.searchVocabulary
    matchQueryToken queryToken =
      mapMaybe (mkTokenMatch lexicon queryToken) storedTokens

mkTokenMatch :: SynonymLexicon -> SearchToken -> SearchToken -> Maybe QueryTokenMatch
mkTokenMatch lexicon queryToken storedToken =
  case classifyTokenMatch lexicon queryToken storedToken of
    Just (matchKind, matchDistance) ->
      Just
        QueryTokenMatch
          { matchedQueryToken = queryToken,
            matchedStoredToken = storedToken,
            matchedKind = matchKind,
            matchedDistance = matchDistance,
            matchedQuality = tokenMatchQuality matchKind matchDistance
          }
    Nothing ->
      let distance = tokenEditDistance queryToken storedToken
          threshold = maxTokenDistance (T.length queryToken.unSearchToken)
       in if distance <= threshold
            then
              Just
                QueryTokenMatch
                  { matchedQueryToken = queryToken,
                    matchedStoredToken = storedToken,
                    matchedKind = TokenMatchFuzzy,
                    matchedDistance = distance,
                    matchedQuality = tokenMatchQuality TokenMatchFuzzy distance
                  }
            else Nothing

classifyTokenMatch :: SynonymLexicon -> SearchToken -> SearchToken -> Maybe (TokenMatchKind, Int)
classifyTokenMatch lexicon queryToken storedToken
  | queryToken == storedToken =
      Just (TokenMatchExact, 0)
  | canonicalQueryToken == canonicalStoredToken =
      Just (TokenMatchCanonical, 0)
  | areDirectSynonyms lexicon canonicalQueryToken canonicalStoredToken =
      Just (TokenMatchSynonym, 1)
  | otherwise =
      Nothing
  where
    canonicalQueryToken = canonicalizeSearchToken queryToken
    canonicalStoredToken = canonicalizeSearchToken storedToken

candidateNamesFromMatches :: SymbolSearchIndex -> [QueryTokenMatch] -> Set.Set GHC.Name
candidateNamesFromMatches index matches =
  Set.unions
    [ Map.findWithDefault Set.empty match.matchedStoredToken fieldPostings
    | match <- matches,
      fieldPostings <- Map.elems index.searchPostings
    ]

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

scoreSymbolDocument :: SymbolSearchIndex -> SymbolSearchQuery -> [QueryTokenMatch] -> SymbolSearchDocument -> Maybe (NormalizedOccName, Bool, Double, [TokenMatchEvidence])
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
    scoredVariantEvidence :: [TokenMatchEvidence]
  }

scoreNameVariant :: SymbolSearchIndex -> SymbolSearchQuery -> [QueryTokenMatch] -> SymbolSearchDocument -> IndexedNameVariant -> Maybe ScoredNameVariant
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
      mapMaybe (bestEvidenceForQueryToken index matches document nameVariant) query.symbolSearchTokens
    matchedQueryTokens =
      Set.fromList (map (.evidenceQueryToken) selectedEvidence)
    unmatchedCount =
      length [() | token <- query.symbolSearchTokens, token `Set.notMember` matchedQueryTokens]
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

bestEvidenceForQueryToken :: SymbolSearchIndex -> [QueryTokenMatch] -> SymbolSearchDocument -> IndexedNameVariant -> SearchToken -> Maybe TokenMatchEvidence
bestEvidenceForQueryToken index matches document nameVariant queryToken =
  case evidence of
    [] -> Nothing
    _ -> Just (List.minimumBy evidenceOrdering evidence)
  where
    evidence =
      [ mkEvidence match field matchedNameVariant
      | match <- matches,
        match.matchedQueryToken == queryToken,
        (field, matchedNameVariant) <- matchingFields document nameVariant match.matchedStoredToken
      ]
    mkEvidence match field matchedNameVariant =
      TokenMatchEvidence
        { evidenceQueryToken = queryToken,
          evidenceStoredToken = match.matchedStoredToken,
          evidenceField = field,
          evidenceNameVariant = (.indexedName) <$> matchedNameVariant,
          evidenceMatchKind = match.matchedKind,
          evidenceMatchDistance = match.matchedDistance,
          evidenceMatchQuality = match.matchedQuality,
          evidenceIdf = matchTokenIdf index field queryToken match.matchedStoredToken match.matchedKind,
          evidenceContribution = fieldWeight field * match.matchedQuality * matchTokenIdf index field queryToken match.matchedStoredToken match.matchedKind
        }

matchingFields :: SymbolSearchDocument -> IndexedNameVariant -> SearchToken -> [(SymbolSearchField, Maybe IndexedNameVariant)]
matchingFields document nameVariant token =
  nameMatches <> contextMatches
  where
    nameMatches =
      [(SearchName, Just nameVariant) | token `elem` nameVariant.indexedNameTokens]
    contextMatches =
      [ (field, Nothing)
      | field <- [SearchResultType, SearchArgumentType, SearchModule],
        token `Set.member` fieldTokens field document
      ]

evidenceOrdering :: TokenMatchEvidence -> TokenMatchEvidence -> Ordering
evidenceOrdering =
  comparing
    ( \evidence ->
        ( Down evidence.evidenceContribution,
          fieldPriority evidence.evidenceField,
          matchKindPriority evidence.evidenceMatchKind,
          evidence.evidenceMatchDistance,
          evidence.evidenceStoredToken,
          maybe "" (.unNormalizedOccName) evidence.evidenceNameVariant
        )
    )

orderedNameBonusFor :: SymbolSearchQuery -> IndexedNameVariant -> [TokenMatchEvidence] -> Double
orderedNameBonusFor query nameVariant evidence
  | length query.symbolSearchTokens < 2 = 0
  | otherwise =
      variantBonus
  where
    evidenceByQueryToken =
      Map.fromList
        [ (item.evidenceQueryToken, item.evidenceStoredToken)
        | item <- evidence,
          item.evidenceField == SearchName
        ]
    variantBonus =
      let matchingStoredTokens =
            [ storedToken
            | queryToken <- query.symbolSearchTokens,
              Just storedToken <- [Map.lookup queryToken evidenceByQueryToken],
              storedToken `elem` nameVariant.indexedNameTokens
            ]
       in if length matchingStoredTokens >= 2 && tokensMatchInOrder nameVariant.indexedNameTokens matchingStoredTokens
            then orderedNameWeight * fromIntegral (length matchingStoredTokens) / fromIntegral (length query.symbolSearchTokens)
            else 0

nameSpecificityBonusFor :: IndexedNameVariant -> [TokenMatchEvidence] -> Double
nameSpecificityBonusFor nameVariant evidence =
  if totalTokens == 0
    then 0
    else nameSpecificityWeight * fromIntegral (Set.size matchedNameTokens) / fromIntegral totalTokens
  where
    matchedNameTokens =
      Set.fromList
        [ item.evidenceStoredToken
        | item <- evidence,
          item.evidenceNameVariant == Just nameVariant.indexedName
        ]
    totalTokens = length (Set.toList (Set.fromList nameVariant.indexedNameTokens))

scoreBreakdownTotal :: SymbolScoreBreakdown -> Double
scoreBreakdownTotal breakdown =
  breakdown.matchedEvidenceScore
    - breakdown.unmatchedTokenPenalty
    + breakdown.orderedNameBonus
    + breakdown.nameSpecificityBonus
    - breakdown.capitalizationPenalty

tokensMatchInOrder :: [SearchToken] -> [SearchToken] -> Bool
tokensMatchInOrder candidateTokens matchedTokens =
  go candidateTokens matchedTokens
  where
    go _ [] = True
    go [] _ = False
    go (candidateToken : remainingCandidates) tokens@(matchedToken : remainingMatched)
      | candidateToken == matchedToken = go remainingCandidates remainingMatched
      | otherwise = go remainingCandidates tokens

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

matchTokenIdf :: SymbolSearchIndex -> SymbolSearchField -> SearchToken -> SearchToken -> TokenMatchKind -> Double
matchTokenIdf index field queryToken storedToken matchKind =
  case matchKind of
    TokenMatchExact ->
      storedTokenIdf
    TokenMatchCanonical ->
      storedTokenIdf
    TokenMatchSynonym ->
      approximateIdf
    TokenMatchFuzzy ->
      approximateIdf
  where
    storedTokenIdf = tokenIdf index field storedToken
    queryTokenIdf = maybe 1.0 id (tokenIdfIfPresent index field queryToken)
    approximateIdf = min storedTokenIdf queryTokenIdf

wholeNameDistance :: Text -> Text -> Int
wholeNameDistance query lookupName =
  EditDistance.restrictedDamerauLevenshteinDistance
    EditDistance.defaultEditCosts
    (T.unpack (T.toLower query))
    (T.unpack (T.toLower lookupName))

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

tokenMatchQuality :: TokenMatchKind -> Int -> Double
tokenMatchQuality matchKind distance =
  case matchKind of
    TokenMatchExact -> 1.0
    TokenMatchCanonical -> 0.95
    TokenMatchSynonym -> 0.85
    TokenMatchFuzzy -> 1 / (1 + fromIntegral distance)

fieldWeight :: SymbolSearchField -> Double
fieldWeight = \case
  SearchName -> 1.0
  SearchResultType -> 0.65
  SearchArgumentType -> 0.65
  SearchModule -> 0.45

fieldPriority :: SymbolSearchField -> Int
fieldPriority = \case
  SearchName -> 0
  SearchResultType -> 1
  SearchArgumentType -> 2
  SearchModule -> 3

matchKindPriority :: TokenMatchKind -> Int
matchKindPriority = \case
  TokenMatchExact -> 0
  TokenMatchCanonical -> 1
  TokenMatchSynonym -> 2
  TokenMatchFuzzy -> 3

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

dedupePreservingOrder :: (Ord a) => [a] -> [a]
dedupePreservingOrder =
  reverse . snd . List.foldl' keep (Set.empty, [])
  where
    keep (seen, kept) item
      | item `Set.member` seen = (seen, kept)
      | otherwise = (Set.insert item seen, item : kept)

unmatchedTokenPenaltyWeight :: Double
unmatchedTokenPenaltyWeight = 0.75

orderedNameWeight :: Double
orderedNameWeight = 0.5

nameSpecificityWeight :: Double
nameSpecificityWeight = 0.6

capitalizationMismatchPenaltyWeight :: Double
capitalizationMismatchPenaltyWeight = 0.6
