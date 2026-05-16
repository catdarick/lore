module Lore.Internal.Lookup.Search.Tokenize
  ( tokenizeSearchText,
    canonicalizeSearchToken,
    tokenSynonymRepresentative,
  )
where

import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.Lookup.Search.Synonyms (synonymRepresentatives)
import Lore.Internal.Lookup.Search.Types (SearchToken (..))

tokenizeSearchText :: Text -> [SearchToken]
tokenizeSearchText text =
  case concatMap tokenizeChunk (textChunks text) of
    [] ->
      [SearchToken loweredText | not (T.null loweredText)]
    tokens ->
      tokens
  where
    loweredText = T.toLower text

textChunks :: Text -> [Text]
textChunks =
  filter (not . T.null)
    . map T.pack
    . splitWhen isTokenSeparator
    . T.unpack

tokenizeChunk :: Text -> [SearchToken]
tokenizeChunk chunk
  | T.all (not . isAlphaNum) chunk =
      [SearchToken (T.toLower chunk)]
  | otherwise =
      map SearchToken $
        filter isUsefulToken $
          map T.toLower $
            splitCamelAcronymChunk chunk

isUsefulToken :: Text -> Bool
isUsefulToken token =
  T.length token >= 2 || T.all (not . isAlphaNum) token

splitCamelAcronymChunk :: Text -> [Text]
splitCamelAcronymChunk =
  map T.pack . go [] [] . T.unpack
  where
    go completed current [] =
      reverse (reverse current : completed)
    go completed current [char] =
      reverse (reverse (char : current) : completed)
    go completed current (char : next : rest)
      | isBoundaryAfter char next rest =
          go (reverse (char : current) : completed) [] (next : rest)
      | otherwise =
          go completed (char : current) (next : rest)

isBoundaryAfter :: Char -> Char -> [Char] -> Bool
isBoundaryAfter char next rest =
  (isLower char && isUpper next)
    || (isAlphaNum char && isAlphaNum next && isDigitBoundary char next)
    || case rest of
      following : _ ->
        isUpper char && isUpper next && isLower following
      [] ->
        False

isDigitBoundary :: Char -> Char -> Bool
isDigitBoundary char next =
  (isDigitChar char && not (isDigitChar next))
    || (not (isDigitChar char) && isDigitChar next)

isDigitChar :: Char -> Bool
isDigitChar char =
  char >= '0' && char <= '9'

isTokenSeparator :: Char -> Bool
isTokenSeparator char =
  isSpace char || char == '_' || char == '-' || char == '.' || char == '\''

canonicalizeSearchToken :: SearchToken -> SearchToken
canonicalizeSearchToken (SearchToken token) =
  SearchToken (canonicalizeTokenText token)

tokenSynonymRepresentative :: SearchToken -> Maybe SearchToken
tokenSynonymRepresentative (SearchToken token) =
  SearchToken <$> Map.lookup token synonymRepresentatives

canonicalizeTokenText :: Text -> Text
canonicalizeTokenText token
  | Just canonical <- Map.lookup token irregularPluralCanonicals =
      canonical
  | T.length token <= 3 =
      token
  | Just stem <- stripPluralIes token =
      stem
  | Just stem <- stripPluralEs token =
      stem
  | Just stem <- stripPluralS token =
      stem
  | otherwise =
      token

stripPluralIes :: Text -> Maybe Text
stripPluralIes token
  | T.length token <= 4 =
      Nothing
  | "ies" `T.isSuffixOf` token && not (T.isSuffixOf "eies" token) =
      Just (T.dropEnd 3 token <> "y")
  | otherwise =
      Nothing

stripPluralEs :: Text -> Maybe Text
stripPluralEs token
  | T.length token <= 4 =
      Nothing
  | any (`T.isSuffixOf` token) pluralEsSuffixes =
      Just (T.dropEnd 2 token)
  | otherwise =
      Nothing
  where
    pluralEsSuffixes = ["ches", "shes", "sses", "xes", "zes"]

stripPluralS :: Text -> Maybe Text
stripPluralS token
  | T.length token <= 4 =
      Nothing
  | not ("s" `T.isSuffixOf` token) =
      Nothing
  | any (`T.isSuffixOf` token) ["ss", "us", "is"] =
      Nothing
  | otherwise =
      Just (T.dropEnd 1 token)

irregularPluralCanonicals :: Map.Map Text Text
irregularPluralCanonicals =
  Map.fromList
    [ ("analyses", "analysis"),
      ("appendices", "appendix"),
      ("children", "child"),
      ("criteria", "criterion"),
      ("diagnoses", "diagnosis"),
      ("feet", "foot"),
      ("geese", "goose"),
      ("indices", "index"),
      ("matrices", "matrix"),
      ("men", "man"),
      ("mice", "mouse"),
      ("people", "person"),
      ("phenomena", "phenomenon"),
      ("radii", "radius"),
      ("syntheses", "synthesis"),
      ("teeth", "tooth"),
      ("theses", "thesis"),
      ("vertices", "vertex"),
      ("women", "woman"),
      ("parentheses", "parenthesis"),
      ("nuclei", "nucleus"),
      ("hypotheses", "hypothesis"),
      ("ellipses", "ellipsis")
    ]

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen predicate =
  foldr step [[]]
  where
    step item chunks@(chunk : rest)
      | predicate item = [] : chunks
      | otherwise = (item : chunk) : rest
    step _ [] =
      []
