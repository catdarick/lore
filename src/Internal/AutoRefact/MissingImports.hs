{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.AutoRefact.MissingImports
  ( suggestMissingImportEdits,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Char (isSpace, toLower)
import Data.List (find, foldl', nubBy, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC as Ghc
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as TyThing
import Internal.AutoRefact.Edit (FileEdit (..))
import Internal.AutoRefact.ImportDecl
  ( ImportShape (..),
    ParsedImport (..),
    parseImports,
    parsedImportEffectiveQualifier,
    parsedImportModuleName,
    parsedImportQualified,
    renderImportDecl,
  )
import Internal.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..))
import Internal.Lookup.Types (ExportedSymbol (..))
import Monad (MonadLore)
import System.FilePath (normalise)

suggestMissingImportEdits :: (MonadLore m) => Map FilePath GHC.ModSummary -> Map Text [ExportedSymbol] -> [Diagnostic] -> m [FileEdit]
suggestMissingImportEdits modSummariesByFile symbolsMap diagnostics =
  concat <$> mapM suggestForFile (Map.toList groupedDiagnostics)
  where
    groupedDiagnostics =
      Map.fromListWith
        (<>)
        [ (normalise spanFile, [diagnostic])
        | diagnostic@Diagnostic {diagnosticSpan = RealDiagnosticSpan Span {spanFile}} <- diagnostics
        ]

    suggestForFile (filePath, fileDiagnostics) =
      case Map.lookup filePath modSummariesByFile of
        Nothing ->
          pure []
        Just summary ->
          Ghc.handleSourceError
            (const (pure []))
            do
              parsedModule <- Ghc.parseModule summary
              let parsedImports = parseImports parsedModule
                  requests = mapMaybe missingImportRequest fileDiagnostics
              plan <- foldM (applyMissingImportRequest parsedImports symbolsMap) emptyImportPlan requests
              pure (renderImportPlan filePath plan)

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

data MissingImportRequest = MissingImportRequest
  { requestMissingSymbol :: MissingSymbol,
    requestPreferredModules :: [Text],
    requestSuggestedImportTargets :: [Text]
  }

data ModuleSelectionDecision
  = SelectModule Text ModuleSelectionReason
  | RejectSelection ModuleRejectionReason

data ModuleSelectionReason
  = UniqueCandidate
  | QualifierMatched
  | ExistingUnqualifiedImportMatched
  | ShortestReexportModule

data ModuleRejectionReason
  = AmbiguousQualifiedCandidates
  | AmbiguousUnqualifiedCandidates
  | NoReexportHeuristicMatch

data ImportRequirement
  = RequireUnqualifiedImport Text Text
  | RequireQualifiedImport Text Text

data PlannedImportUpdate = PlannedImportUpdate
  { updateSpan :: Span,
    updateImport :: ParsedImport,
    updateShape :: ImportShape
  }

data NewImportKey
  = NewUnqualifiedImportKey Text
  | NewQualifiedImportKey Text Text
  deriving (Eq, Ord, Show)

data NewImport = NewImport
  { newImportModule :: Text,
    newImportQualifier :: Maybe Text,
    newImportItems :: [Text]
  }

data ImportPlan = ImportPlan
  { planUpdates :: Map ExistingImportId PlannedImportUpdate,
    planInsertions :: Map NewImportKey NewImport
  }

type ExistingImportId = (Int, Int, Int, Int)

emptyImportPlan :: ImportPlan
emptyImportPlan =
  ImportPlan
    { planUpdates = Map.empty,
      planInsertions = Map.empty
    }

selectModuleForMissingSymbol :: MissingSymbol -> [Text] -> [Text] -> [ParsedImport] -> [ExportedSymbol] -> Maybe Text
selectModuleForMissingSymbol missingSymbol preferredModules suggestedImportTargets parsedImports exportedSymbols =
  selectionToMaybe $
    decideModuleSelection missingSymbol preferredModules suggestedImportTargets parsedImports exportedSymbols

decideModuleSelection :: MissingSymbol -> [Text] -> [Text] -> [ParsedImport] -> [ExportedSymbol] -> ModuleSelectionDecision
decideModuleSelection missingSymbol preferredModules suggestedImportTargets parsedImports exportedSymbols =
  let matchingExportedSymbols =
        filter (matchesMissingKind missingSymbol) exportedSymbols
      candidateModules =
        candidateModulesForSymbols matchingExportedSymbols
      preferredMatches = deduplicateTexts $ filter (`elem` candidateModules) preferredModules
      baseCandidates =
        if null preferredMatches
          then candidateModules
          else preferredMatches
   in decideAmongCandidates missingSymbol suggestedImportTargets parsedImports matchingExportedSymbols baseCandidates

decideAmongCandidates :: MissingSymbol -> [Text] -> [ParsedImport] -> [ExportedSymbol] -> [Text] -> ModuleSelectionDecision
decideAmongCandidates missingSymbol suggestedImportTargets parsedImports matchingExportedSymbols = \case
  [moduleName] ->
    SelectModule moduleName UniqueCandidate
  candidateModules ->
    case missingSymbol.missingQualifier of
      Just qualifier ->
        maybe
          (RejectSelection AmbiguousQualifiedCandidates)
          (\moduleName -> SelectModule moduleName QualifierMatched)
          (selectByQualifier qualifier candidateModules)
      Nothing ->
        decideUnqualifiedReexportSelection suggestedImportTargets parsedImports matchingExportedSymbols

decideUnqualifiedReexportSelection :: [Text] -> [ParsedImport] -> [ExportedSymbol] -> ModuleSelectionDecision
decideUnqualifiedReexportSelection suggestedImportTargets parsedImports exportedSymbols =
  case sameSymbolReexportCandidates exportedSymbols of
    Nothing ->
      RejectSelection AmbiguousUnqualifiedCandidates
    Just candidateModules ->
      case existingUnqualifiedImportCandidate parsedImports candidateModules of
        Just moduleName ->
          SelectModule moduleName ExistingUnqualifiedImportMatched
        Nothing
          | null suggestedImportTargets ->
              maybe
                (RejectSelection NoReexportHeuristicMatch)
                (\moduleName -> SelectModule moduleName ShortestReexportModule)
                (shortestModuleName candidateModules)
          | otherwise ->
              RejectSelection AmbiguousUnqualifiedCandidates

selectionToMaybe :: ModuleSelectionDecision -> Maybe Text
selectionToMaybe = \case
  SelectModule moduleName _ -> Just moduleName
  RejectSelection _ -> Nothing

candidateModulesForSymbols :: [ExportedSymbol] -> [Text]
candidateModulesForSymbols =
  deduplicateTexts
    . concatMap candidateModulesForSymbol

candidateModulesForSymbol :: ExportedSymbol -> [Text]
candidateModulesForSymbol =
  map (T.pack . GHC.moduleNameString . GHC.moduleName) . exportedFrom

existingUnqualifiedImportCandidate :: [ParsedImport] -> [Text] -> Maybe Text
existingUnqualifiedImportCandidate parsedImports candidateModules =
  case filter (hasCompatibleUnqualifiedImport parsedImports) candidateModules of
    [moduleName] -> Just moduleName
    _ -> Nothing

sameSymbolReexportCandidates :: [ExportedSymbol] -> Maybe [Text]
sameSymbolReexportCandidates exportedSymbols =
  case exportedSymbols of
    [exportedSymbol] ->
      let candidateModules =
            candidateModulesForSymbol exportedSymbol
       in if length candidateModules > 1
            then Just candidateModules
            else Nothing
    _ ->
      Nothing

hasCompatibleUnqualifiedImport :: [ParsedImport] -> Text -> Bool
hasCompatibleUnqualifiedImport parsedImports moduleName =
  any isCompatible parsedImports
  where
    isCompatible parsedImport =
      parsedImportModuleName parsedImport == moduleName
        && not (parsedImportQualified parsedImport)
        && parsedImport.parsedImportShape /= HidingImport

shortestModuleName :: [Text] -> Maybe Text
shortestModuleName [] = Nothing
shortestModuleName modules =
  Just $
    foldl1 shorterModule modules
  where
    shorterModule left right =
      compareModuleLength left right
        `pickModule` (left, right)

    compareModuleLength left right =
      compare (T.length left) (T.length right)
        <> compare left right

    pickModule ordering (left, right) =
      case ordering of
        GT -> right
        _ -> left

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

sortOnDescending :: (Ord b) => (a -> b) -> [a] -> [a]
sortOnDescending score =
  sortBy (\left right -> compare (score right) (score left))

missingImportRequest :: Diagnostic -> Maybe MissingImportRequest
missingImportRequest Diagnostic {diagnosticMessage} = do
  let moduleExportDiagnostic = parseModuleDoesNotExport diagnosticMessage
      maybeMissingSymbol =
        parseMissingSymbol diagnosticMessage
          <|> fmap fst moduleExportDiagnostic
  missingSymbol <- maybeMissingSymbol
  pure
    MissingImportRequest
      { requestMissingSymbol = missingSymbol,
        requestPreferredModules =
          maybe [] snd moduleExportDiagnostic,
        requestSuggestedImportTargets =
          parseDiagnosticImportTargets diagnosticMessage
      }

applyMissingImportRequest ::
  (MonadLore m) =>
  [ParsedImport] ->
  Map Text [ExportedSymbol] ->
  ImportPlan ->
  MissingImportRequest ->
  m ImportPlan
applyMissingImportRequest parsedImports symbolsMap plan MissingImportRequest {requestMissingSymbol, requestPreferredModules, requestSuggestedImportTargets} = do
  let matchingExportedSymbols =
        filter (matchesMissingKind requestMissingSymbol) $
          Map.findWithDefault [] requestMissingSymbol.missingName symbolsMap
  case selectModuleForMissingSymbol requestMissingSymbol requestPreferredModules requestSuggestedImportTargets parsedImports matchingExportedSymbols of
    Nothing ->
      pure plan
    Just moduleName -> do
      let selectedSymbol =
            listToMaybe
              [ symbol
              | symbol <- matchingExportedSymbols,
                moduleName `elem` map (T.pack . GHC.moduleNameString . GHC.moduleName) symbol.exportedFrom
              ]
      maybeImportItem <- renderImportItem requestMissingSymbol selectedSymbol
      case buildImportRequirement requestMissingSymbol moduleName maybeImportItem of
        Nothing ->
          pure plan
        Just requirement ->
          pure (applyImportRequirement parsedImports plan requirement)

buildImportRequirement :: MissingSymbol -> Text -> Maybe Text -> Maybe ImportRequirement
buildImportRequirement MissingSymbol {missingQualifier = Just qualifier} moduleName _ =
  Just (RequireQualifiedImport moduleName qualifier)
buildImportRequirement MissingSymbol {missingQualifier = Nothing} moduleName maybeImportItem =
  RequireUnqualifiedImport moduleName <$> maybeImportItem

applyImportRequirement :: [ParsedImport] -> ImportPlan -> ImportRequirement -> ImportPlan
applyImportRequirement parsedImports plan = \case
  RequireUnqualifiedImport moduleName importItem ->
    applyUnqualifiedImportRequirement parsedImports plan moduleName importItem
  RequireQualifiedImport moduleName qualifier ->
    applyQualifiedImportRequirement parsedImports plan moduleName qualifier

applyUnqualifiedImportRequirement :: [ParsedImport] -> ImportPlan -> Text -> Text -> ImportPlan
applyUnqualifiedImportRequirement parsedImports plan moduleName importItem =
  case Map.lookup insertionKey plan.planInsertions of
    Just newImport ->
      plan
        { planInsertions =
            Map.insert
              insertionKey
              newImport {newImportItems = appendUnique newImport.newImportItems [importItem]}
              plan.planInsertions
        }
    Nothing ->
      case findCompatibleUnqualifiedImport parsedImports plan moduleName of
        Just parsedImport ->
          case currentImportShape plan parsedImport of
            OpenImport ->
              plan
            ExplicitImport items ->
              updateExistingImport parsedImport (ExplicitImport (appendUnique items [importItem])) plan
            HidingImport ->
              insertNewImport insertionKey (NewImport moduleName Nothing [importItem]) plan
        Nothing ->
          insertNewImport insertionKey (NewImport moduleName Nothing [importItem]) plan
  where
    insertionKey = NewUnqualifiedImportKey moduleName

applyQualifiedImportRequirement :: [ParsedImport] -> ImportPlan -> Text -> Text -> ImportPlan
applyQualifiedImportRequirement parsedImports plan moduleName qualifier =
  case Map.lookup insertionKey plan.planInsertions of
    Just _ ->
      plan
    Nothing ->
      case findCompatibleQualifiedImport parsedImports plan moduleName qualifier of
        Just parsedImport ->
          case currentImportShape plan parsedImport of
            OpenImport ->
              plan
            ExplicitImport _ ->
              updateExistingImport parsedImport OpenImport plan
            HidingImport ->
              insertNewImport insertionKey (NewImport moduleName (Just qualifier) []) plan
        Nothing ->
          insertNewImport insertionKey (NewImport moduleName (Just qualifier) []) plan
  where
    insertionKey = NewQualifiedImportKey moduleName qualifier

findCompatibleUnqualifiedImport :: [ParsedImport] -> ImportPlan -> Text -> Maybe ParsedImport
findCompatibleUnqualifiedImport parsedImports plan moduleName =
  find isCompatible parsedImports
  where
    isCompatible parsedImport =
      parsedImportModuleName parsedImport == moduleName
        && not (parsedImportQualified parsedImport)
        && currentImportShape plan parsedImport /= HidingImport

findCompatibleQualifiedImport :: [ParsedImport] -> ImportPlan -> Text -> Text -> Maybe ParsedImport
findCompatibleQualifiedImport parsedImports plan moduleName qualifier =
  find isCompatible parsedImports
  where
    isCompatible parsedImport =
      parsedImportModuleName parsedImport == moduleName
        && parsedImportQualified parsedImport
        && parsedImportEffectiveQualifier parsedImport == Just qualifier
        && currentImportShape plan parsedImport /= HidingImport

renderImportPlan :: FilePath -> ImportPlan -> [FileEdit]
renderImportPlan filePath ImportPlan {planUpdates, planInsertions} =
  renderUpdates <> renderInsertions
  where
    renderUpdates =
      [ ReplaceSpanEdit filePath updateSpan (renderImportDecl updateImport.parsedImportDecl updateShape)
      | PlannedImportUpdate {updateSpan, updateImport, updateShape} <- Map.elems planUpdates
      ]
    renderInsertions =
      [ AddImportEdit filePath (renderNewImport newImport)
      | newImport <- Map.elems planInsertions
      ]

renderNewImport :: NewImport -> Text
renderNewImport NewImport {newImportModule, newImportQualifier = Just qualifier} =
  "import qualified "
    <> newImportModule
    <> renderAlias qualifier
  where
    renderAlias alias
      | alias == newImportModule = ""
      | otherwise = " as " <> alias
renderNewImport NewImport {newImportModule, newImportItems} =
  "import " <> newImportModule <> " (" <> T.intercalate ", " newImportItems <> ")"

currentImportShape :: ImportPlan -> ParsedImport -> ImportShape
currentImportShape plan parsedImport =
  maybe parsedImport.parsedImportShape (.updateShape) (Map.lookup (existingImportId parsedImport) plan.planUpdates)

updateExistingImport :: ParsedImport -> ImportShape -> ImportPlan -> ImportPlan
updateExistingImport parsedImport updateShape plan =
  plan
    { planUpdates =
        Map.insert
          (existingImportId parsedImport)
          PlannedImportUpdate
            { updateSpan = parsedImport.parsedImportSpan,
              updateImport = parsedImport,
              updateShape
            }
          plan.planUpdates
    }

insertNewImport :: NewImportKey -> NewImport -> ImportPlan -> ImportPlan
insertNewImport newImportKey newImport plan =
  plan
    { planInsertions =
        Map.insertWith mergeNewImport newImportKey newImport plan.planInsertions
    }

mergeNewImport :: NewImport -> NewImport -> NewImport
mergeNewImport newer older =
  older
    { newImportItems = appendUnique older.newImportItems newer.newImportItems
    }

existingImportId :: ParsedImport -> ExistingImportId
existingImportId ParsedImport {parsedImportSpan = Span {spanStartLine, spanStartCol, spanEndLine, spanEndCol}} =
  (spanStartLine, spanStartCol, spanEndLine, spanEndCol)

appendUnique :: [Text] -> [Text] -> [Text]
appendUnique existing additions =
  foldl'
    (\acc value -> if value `elem` acc then acc else acc <> [value])
    existing
    additions
