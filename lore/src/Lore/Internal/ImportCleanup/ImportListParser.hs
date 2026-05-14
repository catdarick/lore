{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.ImportCleanup.ImportListParser
  ( parseImportListPayload,
  )
where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Lore.Internal.ImportCleanup.Types
  ( ImportItem (..),
    ImportItemChildren (..),
    ImportList,
    ImportName (..),
    ImportNamespace (..),
    SepItem (..),
    SepList (..),
    SourceRange (..),
    WildcardImportChildren (..),
    WithRange (..),
  )
import Text.Megaparsec (Parsec, choice, eof, errorBundlePretty, getOffset, many, match, optional, runParser, satisfy, some, try, (<|>))
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void Text

parseImportListPayload :: Text -> Either Text ImportList
parseImportListPayload payloadText =
  if containsUnsupportedComment payloadText
    then Left "import list payload contains comments"
    else
      first
        (T.pack . errorBundlePretty)
        (runParser importListPayloadParser "<import-list-payload>" payloadText)

importListPayloadParser :: Parser ImportList
importListPayloadParser = do
  parsedList <- parseSepListAllowEmpty itemValueParser
  skipListSpace
  eof
  pure parsedList

itemValueParser :: Parser ImportItem
itemValueParser = do
  (itemText, (namespace, itemHead, itemChildren)) <-
    match do
      namespace <- optional (try (namespacePrefix <* someInlineSpace))
      itemHead <- withRange importNameParser
      itemChildren <- importItemChildrenParser
      pure (namespace, itemHead, itemChildren)
  pure
    ImportItem
      { importItemHead = itemHead,
        importItemNamespace = namespace,
        importItemChildren = itemChildren,
        importItemOriginalText = itemText
      }

importItemChildrenParser :: Parser ImportItemChildren
importItemChildrenParser =
  choice
    [ try wildcardChildrenParser,
      try explicitChildrenParser,
      pure NoImportChildren
    ]

wildcardChildrenParser :: Parser ImportItemChildren
wildcardChildrenParser = do
  skipInlineSpace
  fullStart <- getOffset
  _ <- C.char '('
  skipListSpace
  wildcardStart <- getOffset
  _ <- C.string ".."
  wildcardEnd <- getOffset
  skipListSpace
  _ <- C.char ')'
  fullEnd <- getOffset
  pure
    ( WildcardChildren
        WildcardImportChildren
          { wildcardChildrenFullRange = toRange fullStart fullEnd,
            wildcardChildrenRange = toRange wildcardStart wildcardEnd
          }
    )

explicitChildrenParser :: Parser ImportItemChildren
explicitChildrenParser = do
  skipInlineSpace
  _ <- C.char '('
  parsedChildren <- parseSepListNonEmpty childNameParser
  _ <- C.char ')'
  pure (ExplicitChildren parsedChildren)

childNameParser :: Parser ImportName
childNameParser =
  importNameParser

importNameParser :: Parser ImportName
importNameParser =
  ImportName <$> (parenthesizedOperatorParser <|> bareNameParser)

namespacePrefix :: Parser ImportNamespace
namespacePrefix =
  (TypeNamespace <$ C.string "type")
    <|> (PatternNamespace <$ C.string "pattern")

parenthesizedOperatorParser :: Parser Text
parenthesizedOperatorParser = do
  _ <- C.char '('
  op <- some operatorChar
  _ <- C.char ')'
  pure (T.pack ('(' : op <> [')']))

bareNameParser :: Parser Text
bareNameParser =
  T.pack <$> some (satisfy isBareNameChar)

operatorChar :: Parser Char
operatorChar =
  satisfy (\char -> not (isSpace char) && char /= '(' && char /= ')' && char /= ',')

isBareNameChar :: Char -> Bool
isBareNameChar char =
  not (isSpace char) && char /= ',' && char /= '(' && char /= ')'

parseSepListAllowEmpty :: Parser a -> Parser (SepList a)
parseSepListAllowEmpty elementParser = do
  payloadStart <- getOffset
  skipListSpace
  maybeFirst <- optional (try (sepItemParser elementParser))
  (items, separators, trailingSeparator) <-
    case maybeFirst of
      Nothing ->
        pure ([], [], Nothing)
      Just firstItem ->
        continueSeparated [firstItem] []
  payloadEnd <- getOffset
  pure
    SepList
      { sepListPayloadRange = toRange payloadStart payloadEnd,
        sepListItems = attachSeparators items separators,
        sepListTrailingSeparator = trailingSeparator
      }
  where
    continueSeparated revItems revSeparators = do
      maybeSeparator <- optional (try separatorRangeParser)
      case maybeSeparator of
        Nothing ->
          pure (reverse revItems, reverse revSeparators, Nothing)
        Just separatorRange -> do
          maybeNext <- optional (try (sepItemParser elementParser))
          case maybeNext of
            Nothing ->
              pure (reverse revItems, reverse revSeparators, Just separatorRange)
            Just next ->
              continueSeparated (next : revItems) (separatorRange : revSeparators)

parseSepListNonEmpty :: Parser a -> Parser (SepList a)
parseSepListNonEmpty elementParser = do
  sepList <- parseSepListAllowEmpty elementParser
  case sepList.sepListItems of
    [] ->
      fail "expected at least one item"
    _ ->
      pure sepList

sepItemParser :: Parser a -> Parser (SepItem a)
sepItemParser elementParser = do
  outerStart <- getOffset
  skipInlineSpace
  coreStart <- getOffset
  value <- elementParser
  coreEnd <- getOffset
  skipInlineSpace
  outerEnd <- getOffset
  pure
    SepItem
      { sepItemValue = value,
        sepItemCoreRange = toRange coreStart coreEnd,
        sepItemOuterRange = toRange outerStart outerEnd,
        sepItemSeparatorAfter = Nothing
      }

separatorRangeParser :: Parser SourceRange
separatorRangeParser = do
  skipListSpace
  commaStart <- getOffset
  _ <- C.char ','
  skipListSpace
  separatorEnd <- getOffset
  pure (toRange commaStart separatorEnd)

attachSeparators :: [SepItem a] -> [SourceRange] -> [SepItem a]
attachSeparators items separators =
  zipWith attach items (map Just separators <> repeat Nothing)
  where
    attach item maybeSeparator =
      item {sepItemSeparatorAfter = maybeSeparator}

withRange :: Parser a -> Parser (WithRange a)
withRange parser = do
  start <- getOffset
  value <- parser
  end <- getOffset
  pure
    WithRange
      { wrRange = toRange start end,
        wrValue = value
      }

toRange :: Int -> Int -> SourceRange
toRange start end =
  SourceRange
    { rangeStart = start,
      rangeEnd = end
    }

skipInlineSpace :: Parser ()
skipInlineSpace =
  void (many (satisfy isInlineSpaceChar))

skipListSpace :: Parser ()
skipListSpace =
  void (many (satisfy isSpace))

someInlineSpace :: Parser ()
someInlineSpace =
  void (some (satisfy isInlineSpaceChar))

isInlineSpaceChar :: Char -> Bool
isInlineSpaceChar char =
  char == ' ' || char == '\t'

containsUnsupportedComment :: Text -> Bool
containsUnsupportedComment text =
  go 0
  where
    textLength = T.length text

    go index
      | index + 1 >= textLength =
          False
      | otherwise =
          let char = T.index text index
              nextChar = T.index text (index + 1)
           in (char == '-' && nextChar == '-') || (char == '{' && nextChar == '-') || go (index + 1)
