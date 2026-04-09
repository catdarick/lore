{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.MissingImports.Diagnostic
  ( MissingSymbolKind (..),
    MissingSymbol (..),
    MissingImportRequest (..),
    MissingImportRequestKind (..),
    ResolveMissingImportDetails (..),
    ExtendExistingImportDetails (..),
    missingImportRequestFromDiagnostic,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace, toLower)
import Data.List (nubBy)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Diagnostics (Diagnostic (..))

data MissingSymbolKind
  = MissingThing
  | MissingDataConstructor
  | MissingTypeConstructorOrClass
  deriving (Eq, Ord, Show)

data MissingSymbol = MissingSymbol
  { missingName :: Text,
    missingQualifier :: Maybe Text,
    missingKind :: MissingSymbolKind
  }
  deriving (Eq, Ord, Show)

data MissingImportRequest
  = MissingImportRequest
  { requestMissingSymbol :: MissingSymbol,
    requestKind :: MissingImportRequestKind
  }
  deriving (Eq, Show)

data MissingImportRequestKind
  = ResolveMissingImport ResolveMissingImportDetails
  | ExtendExistingImport ExtendExistingImportDetails
  deriving (Eq, Show)

data ResolveMissingImportDetails = ResolveMissingImportDetails
  { requestPreferredModules :: [Text],
    requestSuggestedImportTargets :: [Text]
  }
  deriving (Eq, Show)

data ExtendExistingImportDetails = ExtendExistingImportDetails
  { requestTargetModule :: Text,
    requestImportItemOverride :: Maybe Text
  }
  deriving (Eq, Show)

missingImportRequestFromDiagnostic :: Diagnostic -> Maybe MissingImportRequest
missingImportRequestFromDiagnostic Diagnostic {diagnosticMessage} = do
  let moduleExportDiagnostic = parseModuleDoesNotExport diagnosticMessage
      maybeMissingSymbol =
        parseMissingSymbol diagnosticMessage
          <|> fmap fst moduleExportDiagnostic
  parseExtendExistingImportRequest diagnosticMessage maybeMissingSymbol
    <|> do
      missingSymbol <- maybeMissingSymbol
      pure
        MissingImportRequest
          { requestMissingSymbol = missingSymbol,
            requestKind =
              ResolveMissingImport $
                ResolveMissingImportDetails
                  { requestPreferredModules = maybe [] snd moduleExportDiagnostic,
                    requestSuggestedImportTargets = parseDiagnosticImportTargets diagnosticMessage
                  }
          }

parseExtendExistingImportRequest :: Text -> Maybe MissingSymbol -> Maybe MissingImportRequest
parseExtendExistingImportRequest diagnosticMessage maybeMissingSymbol =
  parseMissingHasFieldImport diagnosticMessage
    <|> parseGenericImportListExtension diagnosticMessage maybeMissingSymbol

parseMissingHasFieldImport :: Text -> Maybe MissingImportRequest
parseMissingHasFieldImport rawMessage = do
  let message = unifySpaces rawMessage
  guardText "HasField" message
  guardText "Perhaps you want to add" message
  requestedField <- parseRequestedImportItem message
  parentType <- parseHasFieldParentType message requestedField
  importTarget <- parseSingleDiagnosticImportTarget message
  pure
    MissingImportRequest
      { requestMissingSymbol =
          MissingSymbol
            { missingName = requestedField,
              missingQualifier = Nothing,
              missingKind = MissingThing
            },
        requestKind =
          ExtendExistingImport $
            ExtendExistingImportDetails
              { requestTargetModule = importTarget,
                requestImportItemOverride = Just (parentType <> "(..)")
              }
      }

parseGenericImportListExtension :: Text -> Maybe MissingSymbol -> Maybe MissingImportRequest
parseGenericImportListExtension rawMessage maybeMissingSymbol = do
  let message = unifySpaces rawMessage
  guardText "Perhaps you want to add" message
  importTarget <- parseSingleDiagnosticImportTarget message
  missingSymbol <- maybeMissingSymbol
  pure
    MissingImportRequest
      { requestMissingSymbol = missingSymbol,
        requestKind =
          ExtendExistingImport $
            ExtendExistingImportDetails
              { requestTargetModule = importTarget,
                requestImportItemOverride = Nothing
              }
      }

parseRequestedImportItem :: Text -> Maybe Text
parseRequestedImportItem message = do
  (_, suffix) <- nonEmptyBreak "Perhaps you want to add " message
  parseMissingSymbolAfterPrefix "Perhaps you want to add " suffix

parseSingleDiagnosticImportTarget :: Text -> Maybe Text
parseSingleDiagnosticImportTarget =
  listToMaybe . parseDiagnosticImportTargets

parseHasFieldParentType :: Text -> Text -> Maybe Text
parseHasFieldParentType message requestedField = do
  (_, suffix) <- nonEmptyBreak "HasField " message
  hasFieldSuffix <- T.stripPrefix "HasField " suffix
  fieldToken : parentTypeToken : _ <- pure (T.words hasFieldSuffix)
  guardText ("\"" <> requestedField <> "\"") fieldToken
  let parentType = stripTrailingPunctuation parentTypeToken
  if T.null parentType
    then Nothing
    else Just parentType

parseMissingSymbol :: Text -> Maybe MissingSymbol
parseMissingSymbol rawMessage =
  parseMissingSymbolWithPrefixes MissingDataConstructor dataConstructorPrefixes
    <|> parseMissingSymbolWithPrefixes MissingTypeConstructorOrClass typeConstructorPrefixes
    <|> parseMissingSymbolWithPrefixes MissingThing thingPrefixes
  where
    message = unifySpaces rawMessage

    parseMissingSymbolWithPrefixes missingKind prefixes =
      firstJust (`parseMissingSymbolAfterPrefix` message) prefixes
        >>= buildMissingSymbol missingKind

    buildMissingSymbol missingKind symbolText =
      let strippedSymbol = stripOuterParens (T.strip symbolText)
          (missingQualifier, missingName) = splitQualifiedSymbol strippedSymbol
       in if T.null missingName
            then Nothing
            else Just MissingSymbol {missingName, missingQualifier, missingKind}

dataConstructorPrefixes :: [Text]
dataConstructorPrefixes =
  [ "Data constructor not in scope: ",
    "Not in scope: data constructor "
  ]

typeConstructorPrefixes :: [Text]
typeConstructorPrefixes =
  ["Not in scope: type constructor or class "]

thingPrefixes :: [Text]
thingPrefixes =
  [ "Variable not in scope: ",
    "Not in scope: "
  ]

parseMissingSymbolAfterPrefix :: Text -> Text -> Maybe Text
parseMissingSymbolAfterPrefix prefix message = do
  rest <- T.stripPrefix prefix message
  parseSymbolToken rest

parseSymbolToken :: Text -> Maybe Text
parseSymbolToken text =
  extractLeadingQuoted text <|> extractBareSymbol text <|> extractQuoted text

extractBareSymbol :: Text -> Maybe Text
extractBareSymbol text =
  case T.takeWhile (not . isSpace) (T.strip text) of
    "" -> Nothing
    symbolText -> Just (stripTrailingPunctuation symbolText)

stripTrailingPunctuation :: Text -> Text
stripTrailingPunctuation =
  T.dropWhileEnd (`elem` [',', ';'])

splitQualifiedSymbol :: Text -> (Maybe Text, Text)
splitQualifiedSymbol symbolText =
  case T.breakOnEnd "." symbolText of
    (qualifierWithDot, unqualifiedName)
      | not (T.null qualifierWithDot),
        let qualifier = T.dropEnd 1 qualifierWithDot,
        isLikelyQualifier qualifier ->
          (Just qualifier, unqualifiedName)
    _ ->
      (Nothing, symbolText)

isLikelyQualifier :: Text -> Bool
isLikelyQualifier qualifier =
  not (T.null qualifier)
    && all isQualifierSegment (T.splitOn "." qualifier)
  where
    isQualifierSegment segment =
      case T.uncons segment of
        Just (firstChar, rest) ->
          (firstChar == '_' || isUpperLike firstChar) && T.all isQualifierChar rest
        Nothing ->
          False

    isUpperLike ch = ch /= toLower ch
    isQualifierChar ch =
      ch == '_'
        || ch == '\''
        || ch == '-'
        || T.any (== ch) "0123456789"
        || isAlphaLike ch
    isAlphaLike ch = ch == toLower ch || isUpperLike ch

parseModuleDoesNotExport :: Text -> Maybe (MissingSymbol, [Text])
parseModuleDoesNotExport rawMessage = do
  let message = unifySpaces rawMessage
  guardText "does not export" message
  moduleName : symbolText : _ <- pure (quotedSegments message)
  pure
    ( MissingSymbol
        { missingName = symbolText,
          missingQualifier = Nothing,
          missingKind = MissingThing
        },
      [moduleName]
    )

guardText :: Text -> Text -> Maybe ()
guardText needle haystack
  | needle `T.isInfixOf` haystack = Just ()
  | otherwise = Nothing

parseDiagnosticImportTargets :: Text -> [Text]
parseDiagnosticImportTargets rawMessage =
  deduplicateTexts $
    maybeToList singleImportTarget <> multipleImportTargets
  where
    message = unifySpaces rawMessage

    singleImportTarget = do
      (_, suffix) <- nonEmptyBreak "in the import of " message
      parseMissingSymbolAfterPrefix "in the import of " suffix

    multipleImportTargets =
      case nonEmptyBreak "one of these import lists:" message of
        Nothing -> []
        Just (_, suffix) -> quotedSegments suffix

nonEmptyBreak :: Text -> Text -> Maybe (Text, Text)
nonEmptyBreak needle haystack =
  let pair@(_, suffix) = T.breakOn needle haystack
   in if T.null suffix then Nothing else Just pair

extractLeadingQuoted :: Text -> Maybe Text
extractLeadingQuoted text =
  case T.uncons (T.stripStart text) of
    Just (quoteStart, afterOpen)
      | isQuoteChar quoteStart ->
          fst <$> splitQuotedSegment quoteStart afterOpen
    _ ->
      Nothing

extractQuoted :: Text -> Maybe Text
extractQuoted text =
  listToMaybe (quotedSegments text)

quotedSegments :: Text -> [Text]
quotedSegments =
  go []
  where
    go acc remaining =
      case firstQuote remaining of
        Nothing -> reverse acc
        Just (quoteStart, afterOpen) ->
          case splitQuotedSegment quoteStart afterOpen of
            Nothing -> reverse acc
            Just (segment, afterClose) ->
              go (segment : acc) afterClose

splitQuotedSegment :: Char -> Text -> Maybe (Text, Text)
splitQuotedSegment quoteStart =
  go []
  where
    closingQuote = matchingQuoteChar quoteStart

    go acc remaining =
      case T.uncons remaining of
        Nothing ->
          Nothing
        Just (ch, rest)
          | ch == closingQuote,
            canCloseQuotedSegment closingQuote rest ->
              Just (T.pack (reverse acc), rest)
          | otherwise ->
              go (ch : acc) rest

canCloseQuotedSegment :: Char -> Text -> Bool
canCloseQuotedSegment '\'' remaining =
  case T.uncons remaining of
    Nothing -> True
    Just (nextChar, _) -> isQuotedSegmentBoundary nextChar
canCloseQuotedSegment _ _ =
  True

isQuotedSegmentBoundary :: Char -> Bool
isQuotedSegmentBoundary ch =
  isSpace ch || ch `elem` [',', ';', ':', ')', ']', '}', '.']

firstQuote :: Text -> Maybe (Char, Text)
firstQuote text =
  case T.findIndex isQuoteChar text of
    Nothing -> Nothing
    Just index ->
      let quoteStart = T.index text index
       in Just (quoteStart, T.drop (index + 1) text)

matchingQuoteChar :: Char -> Char
matchingQuoteChar quoteStart =
  case quoteStart of
    '‘' -> '’'
    '`' -> '\''
    '\'' -> '\''
    '"' -> '"'
    other -> other

isQuoteChar :: Char -> Bool
isQuoteChar ch =
  ch == '‘' || ch == '`' || ch == '\'' || ch == '"'

stripOuterParens :: Text -> Text
stripOuterParens text
  | T.length text >= 2,
    T.head text == '(',
    T.last text == ')' =
      T.init (T.tail text)
  | otherwise =
      text

unifySpaces :: Text -> Text
unifySpaces =
  T.unwords . T.words

deduplicateTexts :: [Text] -> [Text]
deduplicateTexts =
  nubBy (==)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (value : rest) =
  case f value of
    Just result -> Just result
    Nothing -> firstJust f rest
