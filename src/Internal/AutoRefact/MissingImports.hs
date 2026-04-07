{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.AutoRefact.MissingImports
  ( suggestMissingImportOperations,
  )
where

import Data.List (nubBy, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC as Ghc
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as TyThing
import Internal.AutoRefact.ImportDecl
  ( ImportList (..),
    ParsedImport (..),
    parsedImportQualified,
  )
import Internal.AutoRefact.ImportOps (ImportOperation (..))
import Internal.AutoRefact.MissingImports.Diagnostic
  ( MissingImportRequest (..),
    MissingSymbol (..),
    MissingSymbolKind (..),
    missingImportRequestFromDiagnostic,
  )
import Internal.Diagnostics (Diagnostic)
import qualified Internal.Logger as Log
import Internal.Lookup.Types (ExportedSymbol (..))
import Monad (MonadLore)

suggestMissingImportOperations :: (MonadLore m) => [ParsedImport] -> Map Text [ExportedSymbol] -> [Diagnostic] -> m [ImportOperation]
suggestMissingImportOperations parsedImports symbolsMap diagnostics =
  concat <$> mapM suggestForRequest requests
  where
    requests = mapMaybe missingImportRequestFromDiagnostic diagnostics

    suggestForRequest request@MissingImportRequest {requestMissingSymbol} = do
      let matchingExportedSymbols =
            filter (matchesMissingKind requestMissingSymbol) $
              Map.findWithDefault [] requestMissingSymbol.missingName symbolsMap
          selectionDecision =
            decideModuleSelection
              requestMissingSymbol
              request.requestPreferredModules
              request.requestSuggestedImportTargets
              parsedImports
              matchingExportedSymbols
      case selectionDecision of
        RejectSelection rejectionReason -> do
          Log.debug (renderSelectionRejection requestMissingSymbol rejectionReason matchingExportedSymbols)
          pure []
        SelectModule moduleName selectionReason -> do
          Log.debug (renderSelectionChoice requestMissingSymbol moduleName selectionReason matchingExportedSymbols)
          let selectedSymbol =
                listToMaybe
                  [ symbol
                  | symbol <- matchingExportedSymbols,
                    moduleName `elem` map (T.pack . GHC.moduleNameString . GHC.moduleName) symbol.exportedFrom
                  ]
          maybeImportItem <- renderImportItem requestMissingSymbol selectedSymbol
          pure $
            maybeToList $
              buildImportOperation requestMissingSymbol moduleName maybeImportItem

data ModuleSelectionDecision
  = SelectModule Text ModuleSelectionReason
  | RejectSelection ModuleRejectionReason
  deriving (Eq, Show)

data ModuleSelectionReason
  = UniqueCandidate
  | QualifierMatched
  | ExistingUnqualifiedImportMatched
  | ShortestReexportModule
  deriving (Eq, Show)

data ModuleRejectionReason
  = AmbiguousQualifiedCandidates
  | AmbiguousUnqualifiedCandidates
  | NoReexportHeuristicMatch
  deriving (Eq, Show)

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
      parsedImport.parsedImportModuleName == moduleName
        && not (parsedImportQualified parsedImport)
        && case parsedImport.parsedImportList of
          HidingImport {} -> False
          _ -> True

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

sortOnDescending :: (Ord b) => (a -> b) -> [a] -> [a]
sortOnDescending score =
  sortBy (\left right -> compare (score right) (score left))

renderSelectionChoice :: MissingSymbol -> Text -> ModuleSelectionReason -> [ExportedSymbol] -> String
renderSelectionChoice MissingSymbol {missingName, missingQualifier} moduleName selectionReason exportedSymbols =
  "Auto-refact: selected import module "
    <> T.unpack moduleName
    <> " for "
    <> renderMissingSymbol missingQualifier missingName
    <> " via "
    <> show selectionReason
    <> " from candidates "
    <> show (candidateModulesForSymbols exportedSymbols)

renderSelectionRejection :: MissingSymbol -> ModuleRejectionReason -> [ExportedSymbol] -> String
renderSelectionRejection MissingSymbol {missingName, missingQualifier} rejectionReason exportedSymbols =
  "Auto-refact: skipping import fix for "
    <> renderMissingSymbol missingQualifier missingName
    <> " because "
    <> show rejectionReason
    <> " with candidates "
    <> show (candidateModulesForSymbols exportedSymbols)

renderMissingSymbol :: Maybe Text -> Text -> String
renderMissingSymbol maybeQualifier missingName =
  T.unpack $
    maybe
      missingName
      (\qualifier -> qualifier <> "." <> missingName)
      maybeQualifier

buildImportOperation :: MissingSymbol -> Text -> Maybe Text -> Maybe ImportOperation
buildImportOperation MissingSymbol {missingQualifier = Just qualifier} moduleName _ =
  Just (EnsureQualifiedImport moduleName qualifier)
buildImportOperation MissingSymbol {missingQualifier = Nothing} moduleName maybeImportItem =
  AddUnqualifiedItem moduleName <$> maybeImportItem

maybeToList :: Maybe a -> [a]
maybeToList = \case
  Just value -> [value]
  Nothing -> []
