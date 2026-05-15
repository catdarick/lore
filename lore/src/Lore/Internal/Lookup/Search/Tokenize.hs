module Lore.Internal.Lookup.Search.Tokenize
  ( tokenizeSearchText,
  )
where

import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
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

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen predicate =
  foldr step [[]]
  where
    step item chunks@(chunk : rest)
      | predicate item = [] : chunks
      | otherwise = (item : chunk) : rest
    step _ [] =
      []
