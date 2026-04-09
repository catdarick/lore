{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.MissingImports
  ( suggestMissingImportOperations,
  )
where

import Control.Monad.Reader (asks)
import Data.List (nubBy, sortBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC as Ghc
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as TyThing
import Lore.Internal.AutoRefactor.ImportDecl
  ( ImportList (..),
    ParsedImport (..),
    parsedImportQualified,
  )
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..))
import Lore.Internal.AutoRefactor.MissingImports.Diagnostic
  ( ExtendExistingImportDetails (..),
    MissingImportRequest (..),
    MissingImportRequestKind (..),
    MissingSymbol (..),
    MissingSymbolKind (..),
    ResolveMissingImportDetails (..),
  )
import Lore.Internal.Lookup.Types (ExportedSymbol (..))
import Lore.Internal.Session (SessionContext (customPrelude))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

suggestMissingImportOperations :: (MonadLore m) => [ParsedImport] -> Map Text [ExportedSymbol] -> NonEmpty MissingImportRequest -> m [ImportOperation]
suggestMissingImportOperations parsedImports symbolsMap requests = do
  maybeCustomPrelude <- asks customPrelude
  concat <$> mapM (suggestForRequest maybeCustomPrelude) (NE.toList requests)
  where
    suggestForRequest maybeCustomPrelude request@MissingImportRequest {requestMissingSymbol, requestKind} =
      case requestKind of
        ResolveMissingImport ResolveMissingImportDetails {requestPreferredModules, requestSuggestedImportTargets} -> do
          let matchingExportedSymbols =
                filter (matchesMissingKind requestMissingSymbol) $
                  Map.findWithDefault [] requestMissingSymbol.missingName symbolsMap
              selectionDecision =
                decideModuleSelection
                  requestMissingSymbol
                  (prioritizeCustomPrelude maybeCustomPrelude requestPreferredModules)
                  requestSuggestedImportTargets
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
                  buildResolveImportOperation maybeCustomPrelude request moduleName maybeImportItem
        ExtendExistingImport ExtendExistingImportDetails {requestTargetModule, requestImportItemOverride} ->
          let matchingExportedSymbols =
                filter (matchesMissingKind requestMissingSymbol) $
                  Map.findWithDefault [] requestMissingSymbol.missingName symbolsMap
              selectedTargetModule =
                fromMaybe
                  requestTargetModule
                  (selectCustomPreludeModule maybeCustomPrelude matchingExportedSymbols)
           in case buildExtendExistingImportOperation parsedImports requestMissingSymbol selectedTargetModule of
                Nothing -> do
                  let selectedSymbol =
                        listToMaybe
                          [ symbol
                          | symbol <- matchingExportedSymbols,
                            selectedTargetModule `elem` map (T.pack . GHC.moduleNameString . GHC.moduleName) symbol.exportedFrom
                          ]
                  maybeImportItem <- maybe (renderImportItem requestMissingSymbol selectedSymbol) (pure . Just) requestImportItemOverride
                  if selectedTargetModule /= requestTargetModule
                    then do
                      Log.debug (renderCustomPreludeSelection requestMissingSymbol selectedTargetModule)
                      pure $
                        maybeToList $
                          buildResolveImportOperation maybeCustomPrelude request selectedTargetModule maybeImportItem
                    else do
                      Log.debug $
                        "Auto-refact: skipping import-list extension for "
                          <> renderMissingSymbol requestMissingSymbol.missingQualifier requestMissingSymbol.missingName
                          <> " because MissingTargetImport with target "
                          <> T.unpack requestTargetModule
                      pure []
                Just buildOperation -> do
                  let selectedSymbol =
                        listToMaybe
                          [ symbol
                          | symbol <- matchingExportedSymbols,
                            selectedTargetModule `elem` map (T.pack . GHC.moduleNameString . GHC.moduleName) symbol.exportedFrom
                          ]
                  maybeImportItem <- maybe (renderImportItem requestMissingSymbol selectedSymbol) (pure . Just) requestImportItemOverride
                  pure $
                    maybeToList $
                      buildOperation maybeImportItem

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
  | NoCandidateModules
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
  [] ->
    RejectSelection NoCandidateModules
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

hasCompatibleQualifiedImport :: [ParsedImport] -> Text -> Text -> Bool
hasCompatibleQualifiedImport parsedImports moduleName qualifier =
  any isCompatible parsedImports
  where
    isCompatible parsedImport =
      parsedImport.parsedImportModuleName == moduleName
        && parsedImportQualified parsedImport
        && parsedImport.parsedImportAlias == Just qualifier
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
      pure (fmap (<> "(..)") maybeParent)
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

prioritizeCustomPrelude :: Maybe Text -> [Text] -> [Text]
prioritizeCustomPrelude maybeCustomPrelude preferredModules =
  deduplicateTexts (maybeToList maybeCustomPrelude <> preferredModules)

selectCustomPreludeModule :: Maybe Text -> [ExportedSymbol] -> Maybe Text
selectCustomPreludeModule maybeCustomPrelude exportedSymbols = do
  customPreludeModule <- maybeCustomPrelude
  if customPreludeModule `elem` candidateModulesForSymbols exportedSymbols
    then Just customPreludeModule
    else Nothing

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

renderCustomPreludeSelection :: MissingSymbol -> Text -> String
renderCustomPreludeSelection MissingSymbol {missingName, missingQualifier} moduleName =
  "Auto-refact: selected custom prelude module "
    <> T.unpack moduleName
    <> " for "
    <> renderMissingSymbol missingQualifier missingName

buildResolveImportOperation :: Maybe Text -> MissingImportRequest -> Text -> Maybe Text -> Maybe ImportOperation
buildResolveImportOperation _ MissingImportRequest {requestMissingSymbol = MissingSymbol {missingQualifier = Just qualifier}} moduleName _ =
  Just (EnsureQualifiedImport moduleName qualifier)
buildResolveImportOperation maybeCustomPrelude MissingImportRequest {} moduleName maybeImportItem
  | maybeCustomPrelude == Just moduleName =
      Just (EnsureUnqualifiedOpenImport moduleName)
  | otherwise =
      AddUnqualifiedItem moduleName <$> maybeImportItem

buildExtendExistingImportOperation :: [ParsedImport] -> MissingSymbol -> Text -> Maybe (Maybe Text -> Maybe ImportOperation)
buildExtendExistingImportOperation parsedImports MissingSymbol {missingQualifier = Just qualifier} moduleName
  | hasCompatibleQualifiedImport parsedImports moduleName qualifier =
      Just (const (Just (EnsureQualifiedImport moduleName qualifier)))
buildExtendExistingImportOperation parsedImports MissingSymbol {missingQualifier = Nothing} moduleName
  | hasCompatibleUnqualifiedImport parsedImports moduleName =
      Just (fmap (AddUnqualifiedItemToExistingImport moduleName))
buildExtendExistingImportOperation _ _ _ =
  Nothing
