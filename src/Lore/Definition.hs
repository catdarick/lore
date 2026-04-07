module Lore.Definition
  ( resolveDefinitionSlice,
    resolveDefinitionClosure,
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
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Data.Bag as Bag
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Monad

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

data ModuleContext = ModuleContext
  { parsedModule :: GHC.ParsedModule,
    typecheckedModule :: GHC.TypecheckedModule
  }

data DefinitionAnalysis = DefinitionAnalysis
  { analysisSlice :: DefinitionSlice,
    analysisReferences :: [GHC.Name]
  }

data ResolverCache = ResolverCache
  { cachedModules :: Map.Map GHC.Module ModuleContext,
    cachedAnalyses :: Map.Map GHC.Name (Maybe DefinitionAnalysis)
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

data DefinitionKey = DefinitionKey
  { definitionKeyModule :: GHC.Module,
    definitionKeySpan :: Maybe GHC.RealSrcSpan
  }
  deriving stock (Eq, Ord)

data SourceImport = SourceImport
  { sourceImportId :: Int,
    sourceImportDecl :: GHC.LImportDecl GHC.GhcRn
  }

data ClosureState = ClosureState
  { closureCache :: ResolverCache,
    closureSeen :: Set.Set DefinitionKey,
    closureSlices :: [DefinitionSlice]
  }

resolveDefinitionSlice :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSlice)
resolveDefinitionSlice inputName = do
  ModSummaries modSummaries <- getModSummaries
  (_, analysis) <- resolveDefinitionAnalysis modSummaries emptyResolverCache inputName
  pure (analysisSlice <$> analysis)

resolveDefinitionClosure :: (MonadLore m) => Int -> GHC.Name -> m [DefinitionSlice]
resolveDefinitionClosure maxDepth inputName = do
  ModSummaries modSummaries <- getModSummaries
  let depth = max 0 maxDepth
  result <- go modSummaries depth inputName (ClosureState emptyResolverCache Set.empty [])
  pure (mergeSlicesByModule result.closureSlices)
  where
    go modSummaries depth name state = do
      (cache', analysis) <- resolveDefinitionAnalysis modSummaries state.closureCache name
      case analysis of
        Nothing ->
          pure state {closureCache = cache'}
        Just definitionAnalysis ->
          let slice = analysisSlice definitionAnalysis
              key = definitionKey slice
           in if Set.member key state.closureSeen
                then pure state {closureCache = cache'}
                else
                  if depth == 0
                    then
                      pure
                        state
                          { closureCache = cache',
                            closureSeen = Set.insert key state.closureSeen,
                            closureSlices = state.closureSlices <> [slice]
                          }
                    else do
                      result <-
                        foldlM
                          (go modSummaries (depth - 1))
                          state
                            { closureCache = cache',
                              closureSeen = Set.insert key state.closureSeen,
                              closureSlices = []
                            }
                          definitionAnalysis.analysisReferences
                      pure result {closureSlices = state.closureSlices <> (slice : result.closureSlices)}

    foldlM f = go'
      where
        go' acc [] = pure acc
        go' acc (x : xs) = do
          acc' <- f x acc
          go' acc' xs

mergeDefinitionSlices :: [DefinitionSlice] -> Maybe DefinitionSlice
mergeDefinitionSlices [] = Nothing
mergeDefinitionSlices (slice : slices)
  | all ((== slice.definitionModule) . definitionModule) slices =
      Just
        DefinitionSlice
          { definitionModule = slice.definitionModule,
            declarationSpans =
              sortDeclarationSpans $
                concatMap declarationSpans allSlices,
            requiredImports =
              mergeImports $
                concatMap requiredImports allSlices
          }
  | otherwise =
      Nothing
  where
    allSlices = slice : slices

mergeSlicesByModule :: [DefinitionSlice] -> [DefinitionSlice]
mergeSlicesByModule =
  Map.elems . foldl insertSlice Map.empty
  where
    insertSlice acc slice =
      Map.insertWith mergeTwo slice.definitionModule slice acc

    mergeTwo new old =
      fromMaybe old $ mergeDefinitionSlices [old, new]

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

resolveDefinitionAnalysis ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  ResolverCache ->
  GHC.Name ->
  m (ResolverCache, Maybe DefinitionAnalysis)
resolveDefinitionAnalysis modSummaries cache inputName =
  case Map.lookup inputName cache.cachedAnalyses of
    Just analysis ->
      pure (cache, analysis)
    Nothing ->
      case GHC.nameModule_maybe inputName of
        Nothing ->
          pure (rememberAnalysis Nothing)
        Just definingModule -> do
          (cache', context) <- resolveModuleContext modSummaries cache definingModule
          let analysis = context >>= analyzeDefinition definingModule inputName
          pure
            ( cache'
                { cachedAnalyses =
                    Map.insert inputName analysis cache'.cachedAnalyses
                },
              analysis
            )
  where
    rememberAnalysis analysis =
      ( cache
          { cachedAnalyses =
              Map.insert inputName analysis cache.cachedAnalyses
          },
        analysis
      )

resolveModuleContext ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  ResolverCache ->
  GHC.Module ->
  m (ResolverCache, Maybe ModuleContext)
resolveModuleContext modSummaries cache definingModule =
  case Map.lookup definingModule cache.cachedModules of
    Just context ->
      pure (cache, Just context)
    Nothing ->
      case Map.lookup definingModule modSummaries of
        Nothing ->
          pure (cache, Nothing)
        Just summary -> do
          parsedModule <- GHC.parseModule summary
          typecheckedModule <- GHC.typecheckModule parsedModule
          let context =
                ModuleContext
                  { parsedModule,
                    typecheckedModule
                  }
          pure
            ( cache
                { cachedModules =
                    Map.insert definingModule context cache.cachedModules
                },
              Just context
            )

analyzeDefinition ::
  GHC.Module ->
  GHC.Name ->
  ModuleContext ->
  Maybe DefinitionAnalysis
analyzeDefinition definingModule target ModuleContext {parsedModule, typecheckedModule} = do
  DefinitionMatch {matchedDeclaration, matchedSignature} <-
    findDefinitionMatch target (GHC.pm_parsed_source parsedModule)
  let (tcg, _details) = GHC.tm_internals_ typecheckedModule
      sourceImports =
        [ SourceImport importId importDecl
        | (importId, importDecl) <- zip [0 ..] (GHC.Tc.tcg_rn_imports tcg),
          not (GHC.unLoc importDecl).ideclExt.ideclImplicit
        ]
      spans =
        DeclarationSpans
          { declarationSpan = GHC.getLocA matchedDeclaration,
            signatureSpan = GHC.getLocA <$> matchedSignature
          }
      occurrences =
        collectReferencedOccurrences
          definingModule
          spans
          sourceImports
          tcg
          (GHC.pm_parsed_source parsedModule)
          typecheckedModule
      slice =
        DefinitionSlice
          { definitionModule = definingModule,
            declarationSpans = [spans],
            requiredImports = buildImports sourceImports occurrences
          }
  pure
    DefinitionAnalysis
      { analysisSlice = slice,
        analysisReferences = collectReferencedNames target spans occurrences
      }

collectReferencedOccurrences ::
  GHC.Module ->
  DeclarationSpans ->
  [SourceImport] ->
  GHC.Tc.TcGblEnv ->
  GHC.ParsedSource ->
  GHC.TypecheckedModule ->
  [ReferencedOccurrence]
collectReferencedOccurrences definingModule spans sourceImports tcg parsedSource typecheckedModule =
  case GHC.tm_renamed_source typecheckedModule of
    Nothing ->
      []
    Just (renamedGroup, _imports, _exports, _docs) ->
      dedupeOccurrences $
        mapMaybe toReferencedOccurrence $
          collectLocatedNames targetSpans renamedGroup
  where
    targetSpans =
      declarationSpan spans
        : maybeToList spans.signatureSpan

    occurrenceSyntax =
      collectOccurrenceSyntax targetSpans parsedSource

    toReferencedOccurrence locatedName = do
      gre <- GHC.lookupGRE_Name (GHC.Tc.tcg_rdr_env tcg) (GHC.unLoc locatedName)
      let occurrenceName = GHC.unLoc locatedName
          syntax =
            fromMaybe (OccurrenceSyntax Nothing) $
              lookup (locatedSpan locatedName) occurrenceSyntax
      guardReference definingModule spans occurrenceName $
        ReferencedOccurrence
          { occurrenceName,
            occurrenceParent =
              case GHC.gre_par gre of
                GHC.ParentIs parentName -> Just parentName
                GHC.NoParent -> Nothing,
            occurrenceSyntax = syntax,
            occurrenceCandidates =
              dedupeIds
                [ importId
                | importSpec <- Bag.bagToList (GHC.gre_imp gre),
                  supportsOccurrence syntax (GHC.is_decl importSpec),
                  Just importId <- [findImportId (GHC.is_dloc (GHC.is_decl importSpec))]
                ]
          }

    findImportId importSpan =
      sourceImportId <$> List.find ((== importSpan) . GHC.getLocA . sourceImportDecl) sourceImports

buildImports ::
  [SourceImport] ->
  [ReferencedOccurrence] ->
  [RequiredImport]
buildImports sourceImports occurrences =
  mapMaybe buildRequiredImport $
    IntMap.toAscList assignedOccurrences
  where
    chosenImports =
      chooseMinimalImports importedOccurrences

    importedOccurrences =
      filter (not . null . occurrenceCandidates) occurrences

    assignedOccurrences =
      IntMap.fromListWith
        (<>)
        [ (importId, [ref])
        | ref <- importedOccurrences,
          Just importId <- [List.find (`IntSet.member` chosenImports) ref.occurrenceCandidates]
        ]

    chooseMinimalImports =
      go IntSet.empty
      where
        go chosen [] = chosen
        go chosen remaining =
          let counts =
                IntMap.fromListWith
                  (+)
                  [(c, 1 :: Int) | ref <- remaining, c <- ref.occurrenceCandidates]
              bestImport =
                fst $
                  List.maximumBy (\a b -> compare (snd a) (snd b)) $
                    IntMap.toList counts
              chosen' = IntSet.insert bestImport chosen
           in go chosen' (filter (not . coveredBy chosen') remaining)

        coveredBy chosen ref =
          any (`IntSet.member` chosen) ref.occurrenceCandidates

    buildRequiredImport (importId, refs) = do
      si <- List.find ((== importId) . sourceImportId) sourceImports
      let decl = GHC.unLoc si.sourceImportDecl
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

collectReferencedNames ::
  GHC.Name ->
  DeclarationSpans ->
  [ReferencedOccurrence] ->
  [GHC.Name]
collectReferencedNames target spans =
  dedupeNames
    . filter (isFollowableReference target spans)
    . map (.occurrenceName)

isFollowableReference :: GHC.Name -> DeclarationSpans -> GHC.Name -> Bool
isFollowableReference target spans name =
  name /= target
    && case GHC.nameModule_maybe name of
      Nothing -> False
      Just definingModule ->
        not (definesName spans.declarationSpan definingModule name)

guardReference ::
  GHC.Module ->
  DeclarationSpans ->
  GHC.Name ->
  ReferencedOccurrence ->
  Maybe ReferencedOccurrence
guardReference definingModule spans occurrenceName occurrence
  | definesName spans.declarationSpan definingModule occurrenceName = Nothing
  | otherwise = Just occurrence

definesName :: GHC.SrcSpan -> GHC.Module -> GHC.Name -> Bool
definesName declarationSpan definingModule name =
  GHC.nameModule_maybe name == Just definingModule
    && GHC.nameSrcSpan name `GHC.isSubspanOf` declarationSpan

definitionKey :: DefinitionSlice -> DefinitionKey
definitionKey slice =
  DefinitionKey
    { definitionKeyModule = slice.definitionModule,
      definitionKeySpan =
        GHC.srcSpanToRealSrcSpan . declarationSpan . head $ slice.declarationSpans
    }

emptyResolverCache :: ResolverCache
emptyResolverCache =
  ResolverCache
    { cachedModules = Map.empty,
      cachedAnalyses = Map.empty
    }

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

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  List.sortOn (GHC.srcSpanToRealSrcSpan . declarationSpan)

pkgQualString :: GHC.PkgQual -> Maybe String
pkgQualString = \case
  GHC.NoPkgQual -> Nothing
  pkgQual -> Just (GHC.showSDocUnsafe (GHC.ppr pkgQual))

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans
