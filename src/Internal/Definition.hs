module Internal.Definition
  ( resolveDefinitionSlice,
    mergeDefinitionSlices,
    renderImport,
    DefinitionSlice (..),
    DeclarationSpans (..),
    RequiredImport,
  )
where

import Data.Data (Data, Typeable, cast, gmapQ)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified GHC
import qualified GHC.Data.Bag as Bag
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import Internal.Lookup.ModSummaries (getModSummaries)
import Internal.Lookup.Types (ModSummaries (..))
import Monad

data DefinitionSlice = DefinitionSlice
  { definitionModule :: GHC.Module,
    declarationSpans :: [DeclarationSpans],
    requiredImports :: [RequiredImport]
  }
  deriving stock (Eq)

data DeclarationSpans = DeclarationSpans
  { declarationSpan :: GHC.SrcSpan,
    signatureSpan :: Maybe GHC.SrcSpan
  }
  deriving stock (Eq, Show)

data RequiredImport = RequiredImport
  { importKey :: Int,
    importModule :: GHC.ModuleName,
    importPackageQualifier :: Maybe String,
    importSource :: Bool,
    importQualifiedStyle :: GHC.ImportDeclQualifiedStyle,
    importAlias :: Maybe GHC.ModuleName,
    importOriginallyExplicit :: Bool,
    importItems :: [RequiredImportItem]
  }
  deriving stock (Eq)

data RequiredImportItem
  = ImportName GHC.Name
  | ImportParent GHC.Name [GHC.Name]
  deriving stock (Eq)

data DefinitionMatch = DefinitionMatch
  { matchedDeclaration :: GHC.LHsDecl GHC.GhcPs,
    matchedSignature :: Maybe (GHC.LHsDecl GHC.GhcPs)
  }

newtype OccurrenceSyntax = OccurrenceSyntax
  { syntaxQualifier :: Maybe GHC.ModuleName
  }
  deriving stock (Eq)

data ReferencedOccurrence = ReferencedOccurrence
  { occurrenceName :: GHC.Name,
    occurrenceParent :: Maybe GHC.Name,
    occurrenceSyntax :: OccurrenceSyntax,
    occurrenceCandidates :: [Int]
  }

resolveDefinitionSlice :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSlice)
resolveDefinitionSlice inputName =
  case GHC.nameModule_maybe inputName of
    Nothing ->
      pure Nothing
    Just definingModule -> do
      ModSummaries modSummaries <- getModSummaries
      case Map.lookup definingModule modSummaries of
        Nothing ->
          pure Nothing
        Just summary -> do
          parsedModule <- GHC.parseModule summary
          typecheckedModule <- GHC.typecheckModule parsedModule
          pure $ buildDefinitionSlice definingModule inputName parsedModule typecheckedModule

mergeDefinitionSlices :: [DefinitionSlice] -> Maybe DefinitionSlice
mergeDefinitionSlices [] = Nothing
mergeDefinitionSlices (slice : slices)
  | all ((== slice.definitionModule) . definitionModule) slices =
      Just
        DefinitionSlice
          { definitionModule = slice.definitionModule,
            declarationSpans =
              concatMap declarationSpans allSlices,
            requiredImports =
              mergeImports $
                concatMap requiredImports allSlices
          }
  | otherwise =
      Nothing
  where
    allSlices = slice : slices

renderImport :: RequiredImport -> String
renderImport RequiredImport {..} =
  unwords $
    ["import"]
      <> ["{-# SOURCE #-}" | importSource]
      <> maybe [] pure importPackageQualifier
      <> ["qualified" | importQualifiedStyle == GHC.QualifiedPre]
      <> [modulePart]
      <> maybe [] (\alias -> ["as", GHC.moduleNameString alias]) importAlias
      <> case renderedItems of
        [] -> []
        xs -> ["(" <> List.intercalate ", " (map renderItem xs) <> ")"]
  where
    modulePart =
      GHC.moduleNameString importModule
        <> case importQualifiedStyle of
          GHC.QualifiedPost -> " qualified"
          _ -> ""

    renderedItems
      | isQualifiedImport importQualifiedStyle
          && importAlias /= Nothing
          && not importOriginallyExplicit =
          []
      | otherwise =
          importItems

    renderItem = \case
      ImportName name ->
        renderName name
      ImportParent parentName childNames ->
        renderName parentName
          <> "("
          <> List.intercalate ", " (map renderName childNames)
          <> ")"

    renderName =
      GHC.occNameString . GHC.nameOccName

buildDefinitionSlice ::
  GHC.Module ->
  GHC.Name ->
  GHC.ParsedModule ->
  GHC.TypecheckedModule ->
  Maybe DefinitionSlice
buildDefinitionSlice definingModule target parsedModule typecheckedModule = do
  DefinitionMatch {matchedDeclaration, matchedSignature} <-
    findDefinitionMatch target (GHC.pm_parsed_source parsedModule)
  let spans =
        DeclarationSpans
          { declarationSpan = GHC.getLocA matchedDeclaration,
            signatureSpan = GHC.getLocA <$> matchedSignature
          }
  pure
    DefinitionSlice
      { definitionModule = definingModule,
        declarationSpans = [spans],
        requiredImports =
          buildImports
            [spans]
            (GHC.pm_parsed_source parsedModule)
            typecheckedModule
      }

buildImports ::
  [DeclarationSpans] ->
  GHC.ParsedSource ->
  GHC.TypecheckedModule ->
  [RequiredImport]
buildImports spans parsedSource typecheckedModule =
  case GHC.tm_renamed_source typecheckedModule of
    Nothing ->
      []
    Just (renamedGroup, _imports, _exports, _docs) ->
      mapMaybe buildRequiredImport $
        IntMap.toAscList assignedOccurrences
      where
        targetSpans =
          [ declarationSpan span'
          | span' <- spans
          ]
            <> mapMaybe signatureSpan spans

        (tcg, _details) = GHC.tm_internals_ typecheckedModule

        sourceImports =
          [ (importId, importDecl)
          | (importId, importDecl) <- zip [0 ..] (GHC.Tc.tcg_rn_imports tcg),
            not (GHC.unLoc importDecl).ideclExt.ideclImplicit
          ]

        occurrenceSyntax =
          collectOccurrenceSyntax targetSpans parsedSource

        occurrences =
          dedupeOccurrences $
            mapMaybe toReferencedOccurrence $
              collectLocatedNames targetSpans renamedGroup

        chosenImports =
          chooseMinimalImports occurrences

        assignedOccurrences =
          IntMap.fromListWith
            (<>)
            [ (importId, [ref])
            | ref <- occurrences,
              Just importId <- [List.find (`IntSet.member` chosenImports) ref.occurrenceCandidates]
            ]

        toReferencedOccurrence locatedName = do
          gre <- GHC.lookupGRE_Name (GHC.Tc.tcg_rdr_env tcg) (GHC.unLoc locatedName)
          let syntax =
                fromMaybe (OccurrenceSyntax Nothing) $
                  lookup (locatedSpan locatedName) occurrenceSyntax
              candidates =
                dedupeIds
                  [ importId
                  | importSpec <- Bag.bagToList (GHC.gre_imp gre),
                    supportsOccurrence syntax (GHC.is_decl importSpec),
                    Just importId <- [findImportId (GHC.is_dloc (GHC.is_decl importSpec))]
                  ]
          if null candidates
            then Nothing
            else
              Just
                ReferencedOccurrence
                  { occurrenceName = GHC.unLoc locatedName,
                    occurrenceParent =
                      case GHC.gre_par gre of
                        GHC.ParentIs parentName -> Just parentName
                        GHC.NoParent -> Nothing,
                    occurrenceSyntax = syntax,
                    occurrenceCandidates = candidates
                  }

        findImportId importSpan =
          fst <$> List.find ((== importSpan) . GHC.getLocA . snd) sourceImports

        chooseMinimalImports =
          go IntSet.empty
          where
            go chosen [] = chosen
            go chosen remaining =
              let counts =
                    IntMap.fromListWith
                      (+)
                      [(c, 1 :: Int) | r <- remaining, c <- r.occurrenceCandidates]
                  bestImport =
                    fst $
                      List.maximumBy (\a b -> compare (snd a) (snd b)) $
                        IntMap.toList counts
                  chosen' = IntSet.insert bestImport chosen
               in go chosen' (filter (not . coveredBy chosen') remaining)

            coveredBy chosen ref =
              any (`IntSet.member` chosen) ref.occurrenceCandidates

        buildRequiredImport (importId, refs) = do
          importDecl <- lookup importId sourceImports
          let decl = GHC.unLoc importDecl
          pure
            RequiredImport
              { importKey = importId,
                importModule = GHC.unLoc decl.ideclName,
                importPackageQualifier = pkgQualString decl.ideclPkgQual,
                importSource = decl.ideclSource == GHC.IsBoot,
                importQualifiedStyle = decl.ideclQualified,
                importAlias = GHC.unLoc <$> decl.ideclAs,
                importOriginallyExplicit = decl.ideclImportList /= Nothing,
                importItems = normalizeImportItems (concatMap occurrenceItems refs)
              }

        occurrenceItems ReferencedOccurrence {occurrenceName, occurrenceParent} =
          case occurrenceParent of
            Just parentName
              | parentName /= occurrenceName ->
                  [ImportParent parentName [occurrenceName]]
            _ ->
              [ImportName occurrenceName]

findDefinitionMatch :: GHC.Name -> GHC.ParsedSource -> Maybe DefinitionMatch
findDefinitionMatch target parsedSource = do
  decl <- findParentDeclaration target decls
  pure $ DefinitionMatch decl (findSignatureDeclaration (GHC.nameOccName target) decls)
  where
    decls =
      GHC.hsmodDecls $
        GHC.unLoc parsedSource

findParentDeclaration ::
  GHC.Name ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Maybe (GHC.LHsDecl GHC.GhcPs)
findParentDeclaration target decls =
  case GHC.srcSpanToRealSrcSpan (GHC.nameSrcSpan target) of
    Nothing ->
      Nothing
    Just _ ->
      List.find (\decl -> GHC.nameSrcSpan target `GHC.isSubspanOf` GHC.getLocA decl) decls

findSignatureDeclaration ::
  GHC.OccName ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Maybe (GHC.LHsDecl GHC.GhcPs)
findSignatureDeclaration targetOcc =
  List.find (isTopLevelSignatureFor targetOcc)

matchesLocatedRdrName ::
  GHC.OccName ->
  GHC.GenLocated l GHC.RdrName ->
  Bool
matchesLocatedRdrName targetOcc =
  (== targetOcc) . GHC.rdrNameOcc . GHC.unLoc

isTopLevelSignatureFor :: GHC.OccName -> GHC.LHsDecl GHC.GhcPs -> Bool
isTopLevelSignatureFor targetOcc decl =
  case GHC.unLoc decl of
    GHC.SigD _ sig ->
      signatureMatches targetOcc sig
    GHC.KindSigD _ (GHC.StandaloneKindSig _ name _) ->
      matchesLocatedRdrName targetOcc name
    _ ->
      False

signatureMatches :: GHC.OccName -> GHC.Sig GHC.GhcPs -> Bool
signatureMatches targetOcc = \case
  GHC.TypeSig _ names _ ->
    any (matchesLocatedRdrName targetOcc) names
  GHC.PatSynSig _ names _ ->
    any (matchesLocatedRdrName targetOcc) names
  _ ->
    False

collectOccurrenceSyntax ::
  [GHC.SrcSpan] ->
  GHC.ParsedSource ->
  [(GHC.SrcSpan, OccurrenceSyntax)]
collectOccurrenceSyntax targetSpans parsedSource =
  [ (span', syntaxFromRdrName (GHC.unLoc locatedName))
  | locatedName <- collectTyped parsedSource,
    let span' = locatedSpan locatedName,
    spanWithin targetSpans span'
  ]

collectLocatedNames ::
  (Data a) =>
  [GHC.SrcSpan] ->
  a ->
  [GHC.LocatedN GHC.Name]
collectLocatedNames targetSpans =
  filter (spanWithin targetSpans . locatedSpan)
    . collectTyped

collectTyped :: forall b a. (Typeable b, Data a) => a -> [b]
collectTyped = go
  where
    go :: forall x. (Data x) => x -> [b]
    go value =
      maybeToList (cast value) <> concat (gmapQ go value)

locatedSpan :: GHC.LocatedN a -> GHC.SrcSpan
locatedSpan =
  GHC.locA . GHC.getLoc

syntaxFromRdrName :: GHC.RdrName -> OccurrenceSyntax
syntaxFromRdrName =
  OccurrenceSyntax . fmap fst . GHC.isQual_maybe

supportsOccurrence :: OccurrenceSyntax -> GHC.ImpDeclSpec -> Bool
supportsOccurrence OccurrenceSyntax {syntaxQualifier} importDeclSpec =
  case syntaxQualifier of
    Nothing ->
      not (GHC.is_qual importDeclSpec)
    Just qualifier ->
      GHC.is_as importDeclSpec == qualifier

mergeImports :: [RequiredImport] -> [RequiredImport]
mergeImports =
  IntMap.elems . foldl insertImport IntMap.empty
  where
    insertImport acc import' =
      IntMap.insertWith mergeImport import'.importKey import' acc

    mergeImport new old =
      old
        { importOriginallyExplicit = old.importOriginallyExplicit || new.importOriginallyExplicit,
          importItems = normalizeImportItems (old.importItems <> new.importItems)
        }

normalizeImportItems :: [RequiredImportItem] -> [RequiredImportItem]
normalizeImportItems items =
  standaloneItems <> parentItems
  where
    standaloneNames =
      dedupeNames
        [ name
        | ImportName name <- items
        ]

    childNamesByParent =
      Map.fromListWith
        (<>)
        [ (parentName, childNames)
        | ImportParent parentName childNames <- items
        ]

    standaloneItems =
      map ImportName $
        filter (`Map.notMember` childNamesByParent) standaloneNames

    parentItems =
      [ ImportParent parentName (dedupeNames childNames)
      | (parentName, childNames) <- List.sortOn (renderName . fst) (Map.toList childNamesByParent)
      ]

    renderName =
      GHC.occNameString . GHC.nameOccName

dedupeOccurrences :: [ReferencedOccurrence] -> [ReferencedOccurrence]
dedupeOccurrences =
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.occurrenceName == right.occurrenceName
        && left.occurrenceParent == right.occurrenceParent
        && left.occurrenceSyntax == right.occurrenceSyntax
        && left.occurrenceCandidates == right.occurrenceCandidates

dedupeIds :: [Int] -> [Int]
dedupeIds =
  IntSet.toAscList . IntSet.fromList

dedupeNames :: [GHC.Name] -> [GHC.Name]
dedupeNames =
  Map.elems . Map.fromList . map (\n -> (GHC.occNameString (GHC.nameOccName n), n))

isQualifiedImport :: GHC.ImportDeclQualifiedStyle -> Bool
isQualifiedImport = (/= GHC.NotQualified)

pkgQualString :: GHC.PkgQual -> Maybe String
pkgQualString = \case
  GHC.NoPkgQual -> Nothing
  pkgQual -> Just (GHC.showSDocUnsafe (GHC.ppr pkgQual))

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans
