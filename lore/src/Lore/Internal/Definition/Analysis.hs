module Lore.Internal.Definition.Analysis
  ( collectParsedOccurrenceNames,
    buildParsedModuleFacts,
    buildMinimalTypedModuleFacts,
    buildDefinitionBindings,
    buildDefinitionOccurrences,
    buildReferenceHitsByOccKey,
    buildDependencies,
    buildRequiredImportsById,
    buildDefinitionModuleIndex,
    getMinifiedImportsForDefinition,
    buildUsedInstancesByBinder,
    normalizeImportItems,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data, Typeable, cast, gmapQ)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Data.Bag as Bag
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import qualified GHC.Types.TypeEnv as GHC.TypeEnv
import Lore.Internal.Definition.SourceTree (collectModuleSourceRegionCandidates)
import Lore.Internal.Definition.Types

buildParsedModuleFacts :: GHC.Module -> GHC.ParsedSource -> ParsedModuleFacts
buildParsedModuleFacts definingModule parsedSource =
  ParsedModuleFacts
    { parsedOccKeys =
        Set.fromList
          [ rdrNameOccKey (GHC.unLoc locatedName)
          | locatedName <- locatedRdrNames
          ],
      parsedDeclarationsById = Map.fromList declarationEntries,
      parsedOccurrenceSyntaxBySpan =
        Map.fromListWith keepOldOccurrenceSyntax occurrenceSyntaxEntries,
      parsedRegionCandidates = collectModuleSourceRegionCandidates parsedSource
    }
  where
    decls = GHC.hsmodDecls $ GHC.unLoc parsedSource
    locatedRdrNames = collectLocatedRdrNames parsedSource

    declarationEntries =
      [ (definitionIdFromSpans definingModule declarationSpans, declarationSpans)
      | decl <- decls,
        name <- take 1 (collectTyped decl :: [GHC.LocatedN GHC.RdrName]),
        let declarationSpans =
              DeclarationSpans
                { declarationSpan = GHC.getLocA decl,
                  signatureSpan = GHC.getLocA <$> findSignatureDeclaration (GHC.rdrNameOcc (GHC.unLoc name)) decls
                }
      ]

    occurrenceSyntaxEntries =
      [ (srcSpanKey span', syntaxForOccurrence (GHC.unLoc locatedName))
      | locatedName <- locatedRdrNames,
        let span' = locatedSpan locatedName
      ]

    syntaxForOccurrence rdrName =
      ParsedOccurrenceSyntax
        { parsedSyntaxQualifier = fmap fst (GHC.isQual_maybe rdrName)
        }

    keepOldOccurrenceSyntax _new old =
      old

collectParsedOccurrenceNames :: GHC.ParsedSource -> Set.Set OccKey
collectParsedOccurrenceNames parsedSource =
  Set.fromList
    [ rdrNameOccKey (GHC.unLoc locatedName)
    | locatedName <- collectLocatedRdrNames parsedSource
    ]

buildMinimalTypedModuleFacts ::
  GHC.Module ->
  GHC.Tc.TcGblEnv ->
  MinimalTypedModuleFacts
buildMinimalTypedModuleFacts definingModule tcg =
  MinimalTypedModuleFacts
    { typedDefinitionNames = collectDefinitionCandidateNames definingModule tcg,
      typedSourceImports = collectMinimalTypedImports tcg,
      typedOccurrences = collectMinimalTypedOccurrences tcg
    }

buildDefinitionBindings ::
  GHC.Module ->
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  DefinitionBindings
buildDefinitionBindings definingModule parsedFacts typedModuleFacts =
  DefinitionBindings
    { bindingDefinitionsById = definitionsById,
      bindingDefinitionIdByName = definitionIdByName
    }
  where
    matchedDefinitions =
      [ (definitionId, definitionName)
      | definitionName <- typedModuleFacts.typedDefinitionNames,
        Just definitionId <- [matchDefinitionId definitionName]
      ]

    matchDefinitionId definitionName =
      fst
        <$> List.find
          (\(_, spans) -> GHC.nameSrcSpan definitionName `GHC.isSubspanOf` spans.declarationSpan)
          (Map.toList parsedFacts.parsedDeclarationsById)

    definitionNamesById =
      Map.fromListWith
        (<>)
        [ (definitionId, Set.singleton definitionName)
        | (definitionId, definitionName) <- matchedDefinitions
        ]

    definitionsById =
      Map.mapWithKey mkDefinitionSource definitionNamesById

    mkDefinitionSource definitionId names =
      let spans = parsedFacts.parsedDeclarationsById Map.! definitionId
       in DefinitionSource
            { definitionSourceId = definitionId,
              definitionSourceModule = definingModule,
              definitionSourceNames = names,
              definitionSourceSpans = spans
            }

    definitionIdByName =
      Map.fromList
        [ (definitionName, definitionId)
        | (definitionId, definitionName) <- matchedDefinitions
        ]

buildDefinitionOccurrences ::
  GHC.Module ->
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  DefinitionBindings ->
  Map.Map ImportId ImportCandidate ->
  Map.Map DefinitionId [DefinitionOccurrenceFact]
buildDefinitionOccurrences definingModule parsedFacts typedModuleFacts bindings importCandidatesById =
  Map.map mkOccurrences bindings.bindingDefinitionsById
  where
    mkOccurrences source =
      collectDefinitionOccurrenceFacts
        definingModule
        source.definitionSourceSpans
        parsedFacts.parsedOccurrenceSyntaxBySpan
        importCandidatesById
        typedModuleFacts.typedOccurrences

buildReferenceHitsByOccKey ::
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  Map.Map OccKey [ReferenceHit]
buildReferenceHitsByOccKey occurrencesById =
  Map.fromListWith
    (<>)
    [ (nameOccKey referenceHit.referenceHitTargetName, [referenceHit])
    | (definitionId, occurrences) <- Map.toList occurrencesById,
      occurrence <- occurrences,
      let referenceHit =
            ReferenceHit
              { referenceHitDefinitionId = definitionId,
                referenceHitTargetName = occurrence.occurrenceFactName,
                referenceHitExactSpan = occurrence.occurrenceFactSpan
              }
    ]

buildDependencies ::
  DefinitionBindings ->
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  Maybe MinimalCoreModuleFacts ->
  Map.Map DefinitionId DefinitionDependencies
buildDependencies bindings occurrencesById maybeCoreFacts =
  Map.mapWithKey mkDependencies bindings.bindingDefinitionsById
  where
    coreUsedInstancesByBinder =
      maybe Map.empty (.coreUsedInstancesByBinder) maybeCoreFacts

    mkDependencies definitionId source =
      DefinitionDependencies
        { dependencyDirectReferenceNames =
            Set.fromList
              [ occurrence.occurrenceFactName
              | occurrence <- Map.findWithDefault [] definitionId occurrencesById,
                isFollowableReference source.definitionSourceNames source.definitionSourceSpans occurrence.occurrenceFactName
              ],
          dependencyUsedInstanceNames =
            Set.fromList
              [ instanceName
              | binderName <- Set.toList source.definitionSourceNames,
                instanceName <- Map.findWithDefault [] binderName coreUsedInstancesByBinder
              ]
        }

buildRequiredImportsById ::
  [ImportCandidate] ->
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  Map.Map DefinitionId [RequiredImport]
buildRequiredImportsById importCandidates occurrencesById =
  Map.map (buildMinifiedImports importCandidates) occurrencesById

buildDefinitionModuleIndex ::
  GHC.Module ->
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  Maybe MinimalCoreModuleFacts ->
  DefinitionModuleIndex
buildDefinitionModuleIndex definingModule parsedFacts typedModuleFacts maybeCoreFacts =
  DefinitionModuleIndex
    { definitionsById = bindings.bindingDefinitionsById,
      definitionIdByName = bindings.bindingDefinitionIdByName,
      referenceHitsByOccKey = buildReferenceHitsByOccKey occurrencesById,
      dependenciesById = buildDependencies bindings occurrencesById maybeCoreFacts,
      requiredImportsById = buildRequiredImportsById importCandidates occurrencesById
    }
  where
    bindings =
      buildDefinitionBindings definingModule parsedFacts typedModuleFacts

    importCandidates =
      map minimalImportToImportCandidate typedModuleFacts.typedSourceImports

    importCandidatesById =
      Map.fromList
        [ (importCandidate.importCandidateId, importCandidate)
        | importCandidate <- importCandidates
        ]

    occurrencesById =
      buildDefinitionOccurrences definingModule parsedFacts typedModuleFacts bindings importCandidatesById

minimalImportToImportCandidate :: MinimalTypedImport -> ImportCandidate
minimalImportToImportCandidate minimalImport =
  ImportCandidate
    { importCandidateId = minimalImport.typedImportId,
      importCandidateBaseImport =
        RequiredImport
          { importKey = unImportId minimalImport.typedImportId,
            importModule = minimalImport.typedImportModule,
            importPackageQualifier = minimalImport.typedImportPackageQualifier,
            importSource = minimalImport.typedImportSource,
            importQualifiedStyle = minimalImport.typedImportQualifiedStyle,
            importAlias = minimalImport.typedImportAlias,
            importOriginallyExplicit = minimalImport.typedImportOriginallyExplicit,
            importItems = []
          }
    }

buildUsedInstancesByBinder :: Set.Set GHC.Name -> [GHC.CoreBind] -> Map.Map GHC.Name [GHC.Name]
buildUsedInstancesByBinder interestingBinders coreBinds =
  Map.fromListWith (<>) $
    concatMap bindingEntries coreBinds
  where
    keepEntry binderName instances =
      [ (binderName, instances)
      | Set.member binderName interestingBinders,
        not (null instances)
      ]

    bindingEntries = \case
      GHC.NonRec binder rhs ->
        let instances = dedupeSemanticNamesExact (collectExprUsedInstanceNames rhs)
         in keepEntry (GHC.getName binder) instances
      GHC.Rec pairs ->
        concat
          [ keepEntry (GHC.getName binder) instances
          | (binder, rhs) <- pairs,
            let instances = dedupeSemanticNamesExact (collectExprUsedInstanceNames rhs)
          ]

collectDefinitionCandidateNames :: GHC.Module -> GHC.Tc.TcGblEnv -> [GHC.Name]
collectDefinitionCandidateNames homeModule tcg =
  nubOrd (topLevelNames <> instanceNames)
  where
    belongsToModule name =
      GHC.nameModule_maybe name == Just homeModule

    topLevelNames =
      filter belongsToModule $
        map GHC.getName (GHC.TypeEnv.typeEnvElts (GHC.Tc.tcg_type_env tcg))

    instanceNames =
      filter belongsToModule $
        map GHC.getName (GHC.Tc.tcg_insts tcg)
          <> map GHC.getName (GHC.Tc.tcg_fam_insts tcg)

collectMinimalTypedImports :: GHC.Tc.TcGblEnv -> [MinimalTypedImport]
collectMinimalTypedImports tcg =
  [ MinimalTypedImport
      { typedImportId = ImportId importId,
        typedImportModule = GHC.unLoc decl.ideclName,
        typedImportPackageQualifier = pkgQualString decl.ideclPkgQual,
        typedImportSource = decl.ideclSource == GHC.IsBoot,
        typedImportQualifiedStyle = case decl.ideclQualified of
          GHC.QualifiedPre -> QualifiedPre
          GHC.QualifiedPost -> QualifiedPost
          GHC.NotQualified -> NotQualified,
        typedImportAlias = GHC.unLoc <$> decl.ideclAs,
        typedImportOriginallyExplicit = decl.ideclImportList /= Nothing
      }
  | (importId, importDecl) <- zip [0 ..] (GHC.Tc.tcg_rn_imports tcg),
    let decl = GHC.unLoc importDecl,
    not decl.ideclExt.ideclImplicit
  ]

collectMinimalTypedOccurrences :: GHC.Tc.TcGblEnv -> [MinimalTypedOccurrence]
collectMinimalTypedOccurrences tcg =
  case GHC.Tc.tcg_rn_decls tcg of
    Nothing -> []
    Just renamedGroup ->
      dedupeMinimalTypedOccurrences . mapMaybe toMinimalTypedOccurrence $ collectTyped renamedGroup
  where
    toMinimalTypedOccurrence locatedName = do
      gre <- GHC.lookupGRE_Name (GHC.Tc.tcg_rdr_env tcg) (GHC.unLoc locatedName)
      pure
        MinimalTypedOccurrence
          { typedOccurrenceName = GHC.unLoc locatedName,
            typedOccurrenceSpan = locatedSpan locatedName,
            typedOccurrenceParent = case GHC.gre_par gre of
              GHC.ParentIs parentName -> Just parentName
              GHC.NoParent -> Nothing,
            typedOccurrenceCandidates =
              dedupeImportIds
                [ importId
                | importSpec <- Bag.bagToList (GHC.gre_imp gre),
                  Just importId <- [findImportId (GHC.is_dloc (GHC.is_decl importSpec))]
                ]
          }

    findImportId importSpan =
      ImportId . fst <$> List.find ((== importSpan) . snd) importDeclSpans

    importDeclSpans =
      [ (importId, GHC.getLocA importDecl)
      | (importId, importDecl) <- zip [0 ..] (GHC.Tc.tcg_rn_imports tcg),
        not (GHC.unLoc importDecl).ideclExt.ideclImplicit
      ]

normalizeImportItems :: [RequiredImportItem] -> [RequiredImportItem]
normalizeImportItems items =
  standaloneItems <> parentItems
  where
    standaloneNames =
      dedupeImportItemNamesByRenderedOcc
        [ name
        | ImportName name <- items
        ]

    childNamesByRenderedParent =
      List.foldl'
        insertParentChildren
        Map.empty
        [ (parentName, childNames)
        | ImportParent parentName childNames <- items
        ]

    standaloneItems =
      map ImportName $
        filter ((`Map.notMember` childNamesByRenderedParent) . renderName) standaloneNames

    parentItems =
      [ ImportParent parentName (dedupeImportItemNamesByRenderedOcc childNames)
      | (_, (parentName, childNames)) <- List.sortOn fst (Map.toList childNamesByRenderedParent)
      ]

    renderName =
      GHC.occNameString . GHC.nameOccName

    insertParentChildren parentsByRenderedName (parentName, childNames) =
      Map.insertWith
        mergeParentChildren
        (renderName parentName)
        (parentName, childNames)
        parentsByRenderedName

    mergeParentChildren (_newParentName, newChildNames) (oldParentName, oldChildNames) =
      (oldParentName, oldChildNames <> newChildNames)

collectExprUsedInstanceNames :: GHC.CoreExpr -> [GHC.Name]
collectExprUsedInstanceNames = \case
  GHC.Var variable ->
    [GHC.getName variable | GHC.isDFunId variable]
  GHC.Lit _ ->
    []
  GHC.App function argument ->
    collectExprUsedInstanceNames function <> collectExprUsedInstanceNames argument
  GHC.Lam _ body ->
    collectExprUsedInstanceNames body
  GHC.Let binding body ->
    collectBindUsedInstanceNames binding <> collectExprUsedInstanceNames body
  GHC.Case scrutinee _ _ alternatives ->
    collectExprUsedInstanceNames scrutinee
      <> concatMap collectAltUsedInstanceNames alternatives
  GHC.Cast expression _ ->
    collectExprUsedInstanceNames expression
  GHC.Tick _ expression ->
    collectExprUsedInstanceNames expression
  GHC.Type _ ->
    []
  GHC.Coercion _ ->
    []

collectBindUsedInstanceNames :: GHC.CoreBind -> [GHC.Name]
collectBindUsedInstanceNames = \case
  GHC.NonRec _ rhs ->
    collectExprUsedInstanceNames rhs
  GHC.Rec bindings ->
    concatMap (collectExprUsedInstanceNames . snd) bindings

collectAltUsedInstanceNames :: GHC.CoreAlt -> [GHC.Name]
collectAltUsedInstanceNames (GHC.Alt _ _ expression) =
  collectExprUsedInstanceNames expression

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

collectDefinitionOccurrenceFacts ::
  GHC.Module ->
  DeclarationSpans ->
  Map.Map SpanKey ParsedOccurrenceSyntax ->
  Map.Map ImportId ImportCandidate ->
  [MinimalTypedOccurrence] ->
  [DefinitionOccurrenceFact]
collectDefinitionOccurrenceFacts definingModule spans parsedOccurrenceSyntaxBySpan importCandidatesById typedOccurrences =
  dedupeOccurrences $
    mapMaybe toReferencedOccurrence filteredOccurrences
  where
    targetSpans =
      spans.declarationSpan
        : maybeToList spans.signatureSpan

    filteredOccurrences =
      [ occurrence
      | occurrence <- typedOccurrences,
        spanWithin targetSpans occurrence.typedOccurrenceSpan
      ]

    toReferencedOccurrence occurrence = do
      let occurrenceName = occurrence.typedOccurrenceName
          syntax =
            fromMaybe (ParsedOccurrenceSyntax Nothing) $
              Map.lookup (srcSpanKey occurrence.typedOccurrenceSpan) parsedOccurrenceSyntaxBySpan
      guardReference definingModule spans occurrenceName $
        DefinitionOccurrenceFact
          { occurrenceFactName = occurrenceName,
            occurrenceFactSpan = occurrence.typedOccurrenceSpan,
            occurrenceFactParent = occurrence.typedOccurrenceParent,
            occurrenceFactImportCandidates =
              dedupeImportIds
                [ importId
                | importId <- occurrence.typedOccurrenceCandidates,
                  supportsOccurrence syntax importId
                ]
          }

    supportsOccurrence syntax importId =
      case Map.lookup importId importCandidatesById of
        Nothing -> False
        Just importCandidate -> supportsOccurrenceSyntax syntax importCandidate.importCandidateBaseImport

locatedSpan :: GHC.LocatedN a -> GHC.SrcSpan
locatedSpan =
  GHC.locA . GHC.getLoc

supportsOccurrenceSyntax :: ParsedOccurrenceSyntax -> RequiredImport -> Bool
supportsOccurrenceSyntax ParsedOccurrenceSyntax {parsedSyntaxQualifier} requiredImport =
  case parsedSyntaxQualifier of
    Nothing ->
      requiredImport.importQualifiedStyle == NotQualified
    Just qualifier ->
      requiredImport.importQualifiedStyle /= NotQualified
        && qualifier `elem` supportedQualifiedNames requiredImport
  where
    supportedQualifiedNames import_ =
      case import_.importAlias of
        Just alias -> [alias]
        Nothing -> [import_.importModule]

buildMinifiedImports ::
  [ImportCandidate] ->
  [DefinitionOccurrenceFact] ->
  [RequiredImport]
buildMinifiedImports importCandidates occurrences =
  mapMaybe buildRequiredImport $
    Map.toAscList assignedOccurrences
  where
    chosenImports =
      chooseMinimalImports importedOccurrences

    importedOccurrences =
      filter (not . null . occurrenceFactImportCandidates) occurrences

    assignedOccurrences =
      Map.fromListWith
        (<>)
        [ (candidateId, [ref])
        | ref <- importedOccurrences,
          Just candidateId <- [List.find (`Set.member` chosenImports) ref.occurrenceFactImportCandidates]
        ]

    chooseMinimalImports =
      go Set.empty
      where
        go chosen [] = chosen
        go chosen remaining =
          let counts =
                Map.fromListWith
                  (+)
                  [(candidateId, 1 :: Int) | ref <- remaining, candidateId <- ref.occurrenceFactImportCandidates]
              bestImport =
                fst $
                  List.minimumBy compareImportCandidate $
                    Map.toList counts
              chosen' = Set.insert bestImport chosen
           in go chosen' (filter (not . coveredBy chosen') remaining)

        compareImportCandidate (leftId, leftCount) (rightId, rightCount) =
          compare rightCount leftCount
            <> compare leftId rightId

        coveredBy chosen ref =
          any (`Set.member` chosen) ref.occurrenceFactImportCandidates

    buildRequiredImport (importId, refs) = do
      importCandidate <- List.find ((== importId) . importCandidateId) importCandidates
      pure
        importCandidate.importCandidateBaseImport
          { importItems = normalizeImportItems (concatMap occurrenceItems refs)
          }

    occurrenceItems DefinitionOccurrenceFact {occurrenceFactName, occurrenceFactParent} =
      case occurrenceFactParent of
        Just parentName
          | parentName /= occurrenceFactName ->
              [ImportParent parentName [occurrenceFactName]]
        _ ->
          [ImportName occurrenceFactName]

getMinifiedImportsForDefinition :: DefinitionModuleIndex -> DefinitionId -> [RequiredImport]
getMinifiedImportsForDefinition moduleIndex definitionId =
  Map.findWithDefault [] definitionId moduleIndex.requiredImportsById

isFollowableReference :: Set.Set GHC.Name -> DeclarationSpans -> GHC.Name -> Bool
isFollowableReference definitionNames spans name =
  Set.notMember name definitionNames
    && case GHC.nameModule_maybe name of
      Nothing -> False
      Just definingModule ->
        not (definesName spans.declarationSpan definingModule name)

guardReference ::
  GHC.Module ->
  DeclarationSpans ->
  GHC.Name ->
  DefinitionOccurrenceFact ->
  Maybe DefinitionOccurrenceFact
guardReference definingModule spans occurrenceName occurrence
  | definesName spans.declarationSpan definingModule occurrenceName = Nothing
  | otherwise = Just occurrence

definesName :: GHC.SrcSpan -> GHC.Module -> GHC.Name -> Bool
definesName declarationSpan definingModule name =
  GHC.nameModule_maybe name == Just definingModule
    && GHC.nameSrcSpan name `GHC.isSubspanOf` declarationSpan

pkgQualString :: GHC.PkgQual -> Maybe String
pkgQualString = \case
  GHC.NoPkgQual -> Nothing
  pkgQual -> Just (GHC.showSDocUnsafe (GHC.ppr pkgQual))

dedupeOccurrences :: [DefinitionOccurrenceFact] -> [DefinitionOccurrenceFact]
dedupeOccurrences =
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.occurrenceFactName == right.occurrenceFactName
        && left.occurrenceFactSpan == right.occurrenceFactSpan
        && left.occurrenceFactParent == right.occurrenceFactParent
        && left.occurrenceFactImportCandidates == right.occurrenceFactImportCandidates

dedupeMinimalTypedOccurrences :: [MinimalTypedOccurrence] -> [MinimalTypedOccurrence]
dedupeMinimalTypedOccurrences =
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.typedOccurrenceName == right.typedOccurrenceName
        && left.typedOccurrenceSpan == right.typedOccurrenceSpan
        && left.typedOccurrenceParent == right.typedOccurrenceParent
        && left.typedOccurrenceCandidates == right.typedOccurrenceCandidates

dedupeImportIds :: [ImportId] -> [ImportId]
dedupeImportIds =
  Set.toAscList . Set.fromList

dedupeSemanticNamesExact :: [GHC.Name] -> [GHC.Name]
dedupeSemanticNamesExact =
  dedupeExactNames

dedupeImportItemNamesByRenderedOcc :: [GHC.Name] -> [GHC.Name]
dedupeImportItemNamesByRenderedOcc =
  reverse . snd . foldl go (Set.empty, [])
  where
    go (seen, names) name
      | renderedOccName `Set.member` seen =
          (seen, names)
      | otherwise =
          (Set.insert renderedOccName seen, name : names)
      where
        renderedOccName =
          GHC.occNameString (GHC.nameOccName name)

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans

collectTyped :: forall b a. (Typeable b, Data a) => a -> [b]
collectTyped = go
  where
    go :: forall x. (Data x) => x -> [b]
    go value =
      maybeToList (cast value) <> concat (gmapQ go value)

collectLocatedRdrNames :: GHC.ParsedSource -> [GHC.LocatedN GHC.RdrName]
collectLocatedRdrNames parsedSource =
  collectTyped parsedSource
