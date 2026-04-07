{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.AutoRefact.MissingImports
  ( suggestMissingImportEdits,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace, toLower)
import Data.List (nubBy, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC as Ghc
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as TyThing
import Internal.AutoRefact.Edit (FileEdit (..))
import Internal.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..))
import Internal.Lookup.Types (ExportedSymbol (..))
import Monad (MonadLore)
import System.FilePath (normalise)

suggestMissingImportEdits :: (MonadLore m) => Map Text [ExportedSymbol] -> Diagnostic -> m [FileEdit]
suggestMissingImportEdits symbolsMap Diagnostic {diagnosticSpan, diagnosticMessage} =
  case diagnosticSpan of
    RealDiagnosticSpan Span {spanFile} -> do
      source <- liftIO $ TIO.readFile spanFile
      let moduleExportDiagnostic = parseModuleDoesNotExport diagnosticMessage
          maybeMissingSymbol =
            parseMissingSymbol diagnosticMessage
              <|> fmap fst moduleExportDiagnostic
          matchingExportedSymbols missingSymbol =
            filter (matchesMissingKind missingSymbol) $
              Map.findWithDefault [] missingSymbol.missingName symbolsMap
      case do
        missingSymbol <- maybeMissingSymbol
        let preferredModules =
              parseDiagnosticImportTargets diagnosticMessage
                <> preferredImportedModules missingSymbol source
                <> maybe [] snd moduleExportDiagnostic
        moduleName <-
          selectModuleForMissingSymbol
            missingSymbol
            preferredModules
            (matchingExportedSymbols missingSymbol)
        pure (missingSymbol, moduleName) of
        Nothing ->
          pure []
        Just (missingSymbol, moduleName) -> do
          maybeImportItem <- renderImportItem missingSymbol (listToMaybe (matchingExportedSymbols missingSymbol))
          let existingImportStyle = findExistingImportStyle moduleName source
              renderedImport =
                (\importItem -> renderImportStatement moduleName importItem missingSymbol existingImportStyle)
                  <$> maybeImportItem
          pure $
            maybeToList $
              AddImportEdit
                (normalise spanFile)
                <$> renderedImport
    UnhelpfulDiagnosticSpan {} ->
      pure []

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

selectModuleForMissingSymbol :: MissingSymbol -> [Text] -> [ExportedSymbol] -> Maybe Text
selectModuleForMissingSymbol missingSymbol preferredModules exportedSymbols = do
  let candidateModules =
        deduplicateTexts $
          concatMap
            (map (T.pack . GHC.moduleNameString . GHC.moduleName) . exportedFrom)
            (filter (matchesMissingKind missingSymbol) exportedSymbols)
      preferredMatches = deduplicateTexts $ filter (`elem` candidateModules) preferredModules
      baseCandidates =
        if null preferredMatches
          then candidateModules
          else preferredMatches
  case baseCandidates of
    [moduleName] ->
      Just moduleName
    _ ->
      case missingSymbol.missingQualifier of
        Just qualifier ->
          selectByQualifier qualifier baseCandidates
        Nothing ->
          Nothing

matchesMissingKind :: MissingSymbol -> ExportedSymbol -> Bool
matchesMissingKind MissingSymbol {missingKind = MissingThing} _ = True
matchesMissingKind MissingSymbol {missingKind = MissingDataConstructor} exportedSymbol =
  GHC.isDataOcc (GHC.nameOccName exportedSymbol.name)
matchesMissingKind MissingSymbol {missingKind = MissingTypeConstructorOrClass} exportedSymbol =
  GHC.isTcOcc (GHC.nameOccName exportedSymbol.name)

selectByQualifier :: Text -> [Text] -> Maybe Text
selectByQualifier qualifier modules =
  case sortOnDescending (qualifierImportance qualifier) modules of
    best : next : _
      | qualifierImportance qualifier best > qualifierImportance qualifier next
          && qualifierImportance qualifier best > 0 ->
          Just best
    [best]
      | qualifierImportance qualifier best > 0 ->
          Just best
    _ ->
      Nothing

data ExistingImportStyle = ExistingImportStyle
  { existingImportQualified :: Bool,
    existingImportAlias :: Maybe Text
  }

renderImportStatement :: Text -> Text -> MissingSymbol -> Maybe ExistingImportStyle -> Text
renderImportStatement moduleText importItem MissingSymbol {missingQualifier} existingImportStyle =
  case preferredQualifiedAlias of
    Nothing ->
      "import " <> moduleText <> " (" <> importItem <> ")"
    Just alias ->
      "import qualified "
        <> moduleText
        <> renderAlias alias
  where
    preferredQualifiedAlias =
      case missingQualifier of
        Just qualifier -> Just qualifier
        Nothing ->
          case existingImportStyle of
            Just ExistingImportStyle {existingImportQualified = False} ->
              Nothing
            _ ->
              Nothing

    renderAlias qualifier
      | qualifier == moduleText = ""
      | otherwise = " as " <> qualifier

renderImportItem :: (MonadLore m) => MissingSymbol -> Maybe ExportedSymbol -> m (Maybe Text)
renderImportItem MissingSymbol {missingName, missingKind} maybeExportedSymbol =
  case missingKind of
    MissingDataConstructor -> do
      maybeParent <-
        case maybeExportedSymbol of
          Just exportedSymbol -> resolveParentName exportedSymbol
          Nothing -> pure Nothing
      pure (fmap (\parentName -> parentName <> "(" <> missingName <> ")") maybeParent)
    _ ->
      pure (Just missingName)

resolveParentName :: (MonadLore m) => ExportedSymbol -> m (Maybe Text)
resolveParentName exportedSymbol = do
  maybeTyThing <- Ghc.lookupName exportedSymbol.name
  pure do
    parentTyThing <- maybeTyThing >>= TyThing.tyThingParent_maybe
    pure $
      T.pack $
        GHC.occNameString $
          GHC.nameOccName $
            GHC.getName parentTyThing

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

nonEmptyBreak :: Text -> Text -> Maybe (Text, Text)
nonEmptyBreak needle haystack =
  let pair@(_, suffix) = T.breakOn needle haystack
   in if T.null suffix then Nothing else Just pair

extractLeadingQuoted :: Text -> Maybe Text
extractLeadingQuoted text =
  case T.uncons (T.stripStart text) of
    Just (quoteStart, afterOpen)
      | isQuoteChar quoteStart ->
          case T.breakOn (matchingQuote quoteStart) afterOpen of
            (segment, afterClose)
              | T.null afterClose -> Nothing
              | otherwise -> Just segment
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
          case T.breakOn (matchingQuote quoteStart) afterOpen of
            (segment, afterClose)
              | T.null afterClose -> reverse acc
              | otherwise ->
                  go (segment : acc) (T.drop 1 afterClose)

firstQuote :: Text -> Maybe (Char, Text)
firstQuote text =
  case T.findIndex isQuoteChar text of
    Nothing -> Nothing
    Just index ->
      let quoteStart = T.index text index
       in Just (quoteStart, T.drop (index + 1) text)

matchingQuote :: Char -> Text
matchingQuote quoteStart =
  T.singleton $
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

qualifierImportance :: Text -> Text -> Int
qualifierImportance qualifier moduleText =
  maximum
    [ if loweredQualifier == loweredLastComponent then 100 else 0,
      if loweredQualifier == T.take 1 loweredLastComponent then 85 else 0,
      if loweredQualifier == loweredInitials then 75 else 0,
      if loweredQualifier `T.isSuffixOf` loweredInitials then 65 else 0,
      if loweredQualifier `T.isInfixOf` loweredModule then 40 else 0
    ]
  where
    loweredQualifier = T.toLower qualifier
    loweredModule = T.toLower moduleText
    moduleComponents = filter (not . T.null) (T.splitOn "." loweredModule)
    loweredLastComponent = lastOrEmpty moduleComponents
    loweredInitials = T.concat (map (T.take 1) moduleComponents)

lastOrEmpty :: [Text] -> Text
lastOrEmpty [] = ""
lastOrEmpty xs = last xs

deduplicateTexts :: [Text] -> [Text]
deduplicateTexts =
  nubBy (==)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (value : rest) =
  case f value of
    Just result -> Just result
    Nothing -> firstJust f rest

findExistingImportStyle :: Text -> Text -> Maybe ExistingImportStyle
findExistingImportStyle moduleText =
  listToMaybe
    . mapMaybe parseImportLine
    . T.lines
  where
    parseImportLine line
      | not ("import " `T.isPrefixOf` T.stripStart line) = Nothing
      | moduleText `notElem` tokens = Nothing
      | otherwise =
          Just
            ExistingImportStyle
              { existingImportQualified = "qualified" `elem` tokens,
                existingImportAlias = parseAlias tokens
              }
      where
        tokens = T.words line

    parseAlias ("as" : alias : _) = Just (T.takeWhile (/= '(') alias)
    parseAlias (_ : rest) = parseAlias rest
    parseAlias [] = Nothing

sortOnDescending :: (Ord b) => (a -> b) -> [a] -> [a]
sortOnDescending score =
  sortBy (\left right -> compare (score right) (score left))

parseImportedModules :: Text -> [Text]
parseImportedModules =
  deduplicateTexts
    . mapMaybe parseImportedModule
    . T.lines
  where
    parseImportedModule line = do
      guardText "import " (T.stripStart line)
      findFirst isLikelyModuleName (drop 1 (T.words line))

    isLikelyModuleName token =
      case T.uncons token of
        Just (firstChar, _) ->
          firstChar /= '('
            && firstChar /= '"'
            && firstChar /= '\''
            && firstChar /= '{'
            && firstChar /= '#'
            && firstChar /= '-'
            && firstChar /= ','
            && firstChar /= ')'
            && firstChar /= '_'
            && firstChar /= '['
            && firstChar /= ']'
            && firstChar /= '='
            && firstChar /= ':'
            && firstChar /= ';'
            && firstChar /= '`'
            && firstChar /= '.'
            && firstChar /= '/'
            && firstChar /= '\\'
            && firstChar /= '@'
            && firstChar /= '!'
            && firstChar /= '?'
            && firstChar /= '&'
            && firstChar /= '|'
            && firstChar /= '+'
            && firstChar /= '*'
            && firstChar /= '<'
            && firstChar /= '>'
            && firstChar /= '~'
            && firstChar /= '$'
            && firstChar /= '%'
            && firstChar /= '^'
            && (firstChar == '_' || firstChar /= toLower firstChar)
            && token `notElem` ["qualified", "safe", "as", "hiding", "import"]
        Nothing ->
          False

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst predicate (value : rest)
  | predicate value = Just value
  | otherwise = findFirst predicate rest

preferredImportedModules :: MissingSymbol -> Text -> [Text]
preferredImportedModules MissingSymbol {missingQualifier = Just _} _ =
  []
preferredImportedModules MissingSymbol {missingQualifier = Nothing} source =
  parseImportedModules source
