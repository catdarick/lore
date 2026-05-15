module Lore.Internal.Definition.Analysis
  ( collectParsedOccurrenceNames,
    buildParsedModuleFacts,
    buildMinimalTypedModuleFacts,
    buildDefinitionBindings,
    buildDefinitionMemberIndexes,
    buildDefinitionOccurrences,
    buildReferenceHitsByOccKey,
    buildDependencies,
    buildDefinitionModuleIndex,
    buildUsedInstancesByBinder,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data, Typeable, cast, gmapQ)
import Data.Foldable (foldl')
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Data.Bag as Bag
import qualified GHC.Data.Strict as Strict
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import qualified GHC.Types.Avail as GHC
import qualified GHC.Types.FieldLabel as GHC.FieldLabel
import qualified GHC.Types.TypeEnv as GHC.TypeEnv
import Lore.Internal.Definition.RequiredImports
  ( buildImportCandidates,
    buildRequiredImportsById,
    indexImportCandidates,
  )
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
      parsedDefinitionMembersById = Map.fromListWith (<>) definitionMemberEntries,
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

    definitionMemberEntries =
      [ (definitionIdFromSpans definingModule declarationSpans, collectParsedDefinitionMembers decl)
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
      typedDefinitionOccAliases = collectDefinitionOccAliases definingModule tcg,
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
  Map.Map DefinitionId DefinitionMemberIndex ->
  Map.Map ImportId ImportCandidate ->
  Map.Map DefinitionId [DefinitionOccurrenceFact]
buildDefinitionOccurrences definingModule parsedFacts typedModuleFacts bindings memberIndexesById importCandidatesById =
  Map.map mkOccurrences bindings.bindingDefinitionsById
  where
    mkOccurrences source =
      let memberIndex =
            memberIndexesById Map.! source.definitionSourceId
       in collectDefinitionOccurrenceFacts
            definingModule
            source.definitionSourceSpans
            memberIndex
            parsedFacts.parsedOccurrenceSyntaxBySpan
            importCandidatesById
            typedModuleFacts.typedOccurrences

buildDefinitionMemberIndexes ::
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  DefinitionBindings ->
  Map.Map DefinitionId DefinitionMemberIndex
buildDefinitionMemberIndexes parsedFacts typedModuleFacts bindings =
  Map.map
    ( \source ->
        resolveDefinitionMemberIndex
          source
          parsedFacts.parsedDefinitionMembersById
          typedModuleFacts.typedDefinitionOccAliases
    )
    bindings.bindingDefinitionsById

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
  Map.Map DefinitionId DefinitionMemberIndex ->
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  Maybe MinimalCoreModuleFacts ->
  Map.Map DefinitionId DefinitionDependencies
buildDependencies bindings memberIndexesById occurrencesById maybeCoreFacts =
  Map.mapWithKey mkDependencies bindings.bindingDefinitionsById
  where
    coreUsedInstancesByBinder =
      maybe Map.empty (.coreUsedInstancesByBinder) maybeCoreFacts

    mkDependencies definitionId source =
      let memberIndex =
            Map.findWithDefault
              (DefinitionMemberIndex source.definitionSourceNames [])
              definitionId
              memberIndexesById
          definitionNames = source.definitionSourceNames
          rootNames =
            memberIndex.rootMemberNames
          followableOccurrences =
            [ occurrence
            | occurrence <- Map.findWithDefault [] definitionId occurrencesById,
              isFollowableReference definitionNames source.definitionSourceSpans occurrence.occurrenceFactName
            ]
          directReferencesByReferenceNameRaw =
            Map.fromListWith
              Set.union
              [ (ownerName, Set.singleton occurrence.occurrenceFactName)
              | occurrence <- followableOccurrences,
                ownerName <- ownerNamesForOccurrence definitionNames occurrence
              ]
          usedInstancesByReferenceNameRaw =
            Map.fromListWith
              Set.union
              [ (binderName, Set.singleton instanceName)
              | binderName <- Set.toList definitionNames,
                instanceName <- Map.findWithDefault [] binderName coreUsedInstancesByBinder
              ]
          directReferencesByReferenceName =
            completeDependencyMap
              definitionNames
              rootNames
              directReferencesByReferenceNameRaw
          usedInstancesByReferenceName =
            completeDependencyMap
              definitionNames
              rootNames
              usedInstancesByReferenceNameRaw
       in DefinitionDependencies
            { dependencyDirectReferenceNames =
                Set.unions (Map.elems directReferencesByReferenceName),
              dependencyUsedInstanceNames =
                Set.unions (Map.elems usedInstancesByReferenceName),
              dependencyDirectReferenceNamesByReferenceName = directReferencesByReferenceName,
              dependencyUsedInstanceNamesByReferenceName = usedInstancesByReferenceName
            }

    ownerNamesForOccurrence definitionNames occurrence =
      Set.toList (Set.intersection definitionNames occurrence.occurrenceFactOwners)

    completeDependencyMap definitionNames rootNames rawDependenciesByName =
      augmentRootEntries
        rootNames
        (Set.unions (Map.elems rawDependenciesByName))
        (withDefaultEntries definitionNames rawDependenciesByName)

    withDefaultEntries definitionNames dependenciesByName =
      foldl'
        (\acc definitionName -> Map.insertWith (\_ old -> old) definitionName Set.empty acc)
        dependenciesByName
        (Set.toList definitionNames)

    augmentRootEntries rootNames allDependencies dependenciesByName =
      foldl'
        (\acc rootName -> Map.insertWith Set.union rootName allDependencies acc)
        dependenciesByName
        (Set.toList rootNames)

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
      dependenciesById = buildDependencies bindings memberIndexesById occurrencesById maybeCoreFacts,
      requiredImportsById = buildRequiredImportsById importCandidates occurrencesById
    }
  where
    bindings =
      buildDefinitionBindings definingModule parsedFacts typedModuleFacts

    memberIndexesById =
      buildDefinitionMemberIndexes parsedFacts typedModuleFacts bindings

    importCandidates =
      buildImportCandidates typedModuleFacts.typedSourceImports

    importCandidatesById =
      indexImportCandidates importCandidates

    occurrencesById =
      buildDefinitionOccurrences definingModule parsedFacts typedModuleFacts bindings memberIndexesById importCandidatesById

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
  nubOrd (topLevelNames <> localGreNames <> fieldSelectorNames <> instanceNames)
  where
    belongsToModule name =
      GHC.nameModule_maybe name == Just homeModule

    topLevelNames =
      filter belongsToModule $
        map GHC.getName (GHC.TypeEnv.typeEnvElts (GHC.Tc.tcg_type_env tcg))

    localGreNames =
      filter
        belongsToModule
        [ GHC.greNamePrintableName globalRdrElt.gre_name
        | globalRdrElt <- GHC.globalRdrEnvElts (GHC.Tc.tcg_rdr_env tcg),
          globalRdrElt.gre_lcl
        ]

    fieldSelectorNames =
      filter
        belongsToModule
        [ GHC.flSelector fieldLabel
        | fieldLabels <- GHC.nonDetNameEnvElts (GHC.Tc.tcg_field_env tcg),
          fieldLabel <- fieldLabels
        ]

    instanceNames =
      filter belongsToModule $
        map GHC.getName (GHC.Tc.tcg_insts tcg)
          <> map GHC.getName (GHC.Tc.tcg_fam_insts tcg)

collectDefinitionOccAliases :: GHC.Module -> GHC.Tc.TcGblEnv -> Map.Map GHC.Name (Set.Set Text)
collectDefinitionOccAliases homeModule tcg =
  Map.fromListWith
    Set.union
    [ (selectorName, Set.singleton (fieldLabelAliasText fieldLabel))
    | fieldLabels <- GHC.nonDetNameEnvElts (GHC.Tc.tcg_field_env tcg),
      fieldLabel <- fieldLabels,
      let selectorName = GHC.flSelector fieldLabel,
      GHC.nameModule_maybe selectorName == Just homeModule
    ]

fieldLabelAliasText :: GHC.FieldLabel -> Text
fieldLabelAliasText fieldLabel =
  T.pack (GHC.getOccString (GHC.FieldLabel.fieldLabelPrintableName fieldLabel))

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

collectParsedDefinitionMembers :: GHC.LHsDecl GHC.GhcPs -> [ParsedDefinitionMember]
collectParsedDefinitionMembers decl =
  dedupeParsedDefinitionMembers (constructorMembers <> recordFieldMembers <> classMethodMembers)
  where
    declarationSpan =
      GHC.getLocA decl

    declBody =
      GHC.unLoc decl

    constructorDecls =
      sortBySpanStart GHC.getLocA (declarationConstructors declBody)

    constructorMemberSpans =
      sequentialMemberSpans declarationSpan (map constructorDeclStartSpan constructorDecls)

    constructorMembers =
      [ ParsedDefinitionMember
          { parsedMemberOccKey = rdrNameOccKey (GHC.unLoc constructorName),
            parsedMemberSpan = constructorMemberSpan
          }
      | (constructorDecl, constructorMemberSpan) <- zip constructorDecls constructorMemberSpans,
        constructorName <- constructorDeclaredNames (GHC.unLoc constructorDecl)
      ]

    recordFieldMembers =
      [ ParsedDefinitionMember
          { parsedMemberOccKey = rdrNameOccKey (GHC.unLoc fieldName),
            parsedMemberSpan = GHC.getLocA (GHC.cd_fld_type (GHC.unLoc fieldDecl))
          }
      | constructorDecl <- constructorDecls,
        fieldDecl <- constructorRecordFields (GHC.unLoc constructorDecl),
        fieldOcc <- GHC.cd_fld_names (GHC.unLoc fieldDecl),
        let fieldName = GHC.foLabel (GHC.unLoc fieldOcc)
      ]

    signatureDecls =
      sortBySpanStart GHC.getLocA (declarationClassSignatures declBody)

    signatureMemberSpans =
      sequentialMemberSpans declarationSpan (map GHC.getLocA signatureDecls)

    classMethodMembers =
      [ ParsedDefinitionMember
          { parsedMemberOccKey = rdrNameOccKey (GHC.unLoc methodName),
            parsedMemberSpan = signatureMemberSpan
          }
      | (signatureDecl, signatureMemberSpan) <- zip signatureDecls signatureMemberSpans,
        methodName <- signatureDeclaredNames (GHC.unLoc signatureDecl)
      ]

declarationConstructors :: GHC.HsDecl GHC.GhcPs -> [GHC.LConDecl GHC.GhcPs]
declarationConstructors = \case
  GHC.TyClD _ tyClDecl ->
    case tyClDecl of
      GHC.DataDecl {tcdDataDefn} ->
        dataDefnConstructors tcdDataDefn
      _ ->
        []
  _ ->
    []

dataDefnConstructors :: GHC.HsDataDefn GHC.GhcPs -> [GHC.LConDecl GHC.GhcPs]
dataDefnConstructors dataDefn =
  case dataDefn.dd_cons of
    GHC.NewTypeCon constructorDecl ->
      [constructorDecl]
    GHC.DataTypeCons _ constructorDecls ->
      constructorDecls

declarationClassSignatures :: GHC.HsDecl GHC.GhcPs -> [GHC.LSig GHC.GhcPs]
declarationClassSignatures = \case
  GHC.TyClD _ tyClDecl ->
    case tyClDecl of
      GHC.ClassDecl {tcdSigs} ->
        tcdSigs
      _ ->
        []
  _ ->
    []

constructorDeclaredNames :: GHC.ConDecl GHC.GhcPs -> [GHC.LocatedN GHC.RdrName]
constructorDeclaredNames = \case
  GHC.ConDeclH98 {con_name} ->
    [con_name]
  GHC.ConDeclGADT {con_names} ->
    NE.toList con_names

constructorRecordFields :: GHC.ConDecl GHC.GhcPs -> [GHC.LConDeclField GHC.GhcPs]
constructorRecordFields = \case
  GHC.ConDeclH98 {con_args = GHC.RecCon fields} ->
    GHC.unLoc fields
  GHC.ConDeclGADT {con_g_args = GHC.RecConGADT fields _} ->
    GHC.unLoc fields
  _ ->
    []

constructorDeclStartSpan :: GHC.LConDecl GHC.GhcPs -> GHC.SrcSpan
constructorDeclStartSpan conDecl =
  case constructorDeclaredNames (GHC.unLoc conDecl) of
    firstName : _ ->
      GHC.getLocA firstName
    [] ->
      GHC.getLocA conDecl

signatureDeclaredNames :: GHC.Sig GHC.GhcPs -> [GHC.LocatedN GHC.RdrName]
signatureDeclaredNames = \case
  GHC.TypeSig _ names _ ->
    names
  GHC.ClassOpSig _ _ names _ ->
    names
  GHC.PatSynSig _ names _ ->
    names
  _ ->
    []

sequentialMemberSpans :: GHC.SrcSpan -> [GHC.SrcSpan] -> [GHC.SrcSpan]
sequentialMemberSpans declarationSpan memberStartSpans =
  zipWith spanFromStartToBound memberStartSpans memberBounds
  where
    memberBounds =
      map nextBound (zip [0 :: Int ..] memberStartSpans)

    nextBound (memberIndex, _) =
      case drop (memberIndex + 1) memberStartSpans of
        nextStartSpan : _ ->
          MemberBoundStart nextStartSpan
        [] ->
          MemberBoundEnd declarationSpan

data MemberSpanBound
  = MemberBoundStart GHC.SrcSpan
  | MemberBoundEnd GHC.SrcSpan

spanFromStartToBound :: GHC.SrcSpan -> MemberSpanBound -> GHC.SrcSpan
spanFromStartToBound startSpan endBound =
  case (GHC.srcSpanToRealSrcSpan startSpan, endBoundLocation endBound) of
    (Just startRealSpan, Just endBoundLocation')
      | GHC.srcSpanFile startRealSpan == GHC.srcLocFile endBoundLocation' ->
          GHC.RealSrcSpan
            (GHC.mkRealSrcSpan (GHC.realSrcSpanStart startRealSpan) endBoundLocation')
            Strict.Nothing
    _ ->
      startSpan

endBoundLocation :: MemberSpanBound -> Maybe GHC.RealSrcLoc
endBoundLocation = \case
  MemberBoundStart boundSpan ->
    GHC.realSrcSpanStart <$> GHC.srcSpanToRealSrcSpan boundSpan
  MemberBoundEnd boundSpan ->
    GHC.realSrcSpanEnd <$> GHC.srcSpanToRealSrcSpan boundSpan

sortBySpanStart :: (a -> GHC.SrcSpan) -> [a] -> [a]
sortBySpanStart getSpan =
  List.sortOn (spanStartSortKey . getSpan)

spanStartSortKey :: GHC.SrcSpan -> (String, Int, Int)
spanStartSortKey = \case
  GHC.RealSrcSpan realSpan _ ->
    ( GHC.unpackFS (GHC.srcSpanFile realSpan),
      GHC.srcSpanStartLine realSpan,
      GHC.srcSpanStartCol realSpan
    )
  GHC.UnhelpfulSpan reason ->
    (show reason, maxBound, maxBound)

resolveDefinitionMemberIndex ::
  DefinitionSource ->
  Map.Map DefinitionId [ParsedDefinitionMember] ->
  Map.Map GHC.Name (Set.Set Text) ->
  DefinitionMemberIndex
resolveDefinitionMemberIndex source parsedMembersById definitionOccAliases =
  DefinitionMemberIndex
    { rootMemberNames = rootNames,
      scopedMembers = scopedMembers
    }
  where
    parsedMembers =
      Map.findWithDefault [] source.definitionSourceId parsedMembersById

    scopedMembers =
      dedupeDefinitionMembersByNameSpan $ concatMap resolveParsedMember parsedMembers

    scopedMemberNames =
      Set.fromList (map memberName scopedMembers)

    rootCandidates =
      source.definitionSourceNames `Set.difference` scopedMemberNames

    rootNames
      | Set.null rootCandidates = source.definitionSourceNames
      | otherwise = rootCandidates

    namesByOccKey =
      Map.fromListWith
        (<>)
        [ (occKey, [definitionName])
        | definitionName <- Set.toList source.definitionSourceNames,
          occKey <- definitionNameOccKeys definitionName
        ]

    definitionNameOccKeys definitionName =
      nameOccKey definitionName
        : [ OccKey alias
          | alias <- Set.toList (Map.findWithDefault Set.empty definitionName definitionOccAliases)
          ]

    resolveParsedMember parsedMember =
      [ DefinitionMember definitionName parsedMember.parsedMemberSpan
      | definitionName <- memberNamesForParsedMember parsedMember
      ]

    memberNamesForParsedMember parsedMember =
      dedupeExactNames $
        explicitlyNamedMembers parsedMember
          <> sourceSpannedAliasMembers parsedMember

    explicitlyNamedMembers parsedMember =
      case Map.findWithDefault [] parsedMember.parsedMemberOccKey namesByOccKey of
        [] ->
          []
        [definitionName] ->
          [definitionName]
        candidateNames ->
          let namesWithinSpan = namesWithinMemberSpan candidateNames parsedMember.parsedMemberSpan
           in if null namesWithinSpan then candidateNames else namesWithinSpan

    sourceSpannedAliasMembers parsedMember =
      [ definitionName
      | definitionName <- Set.toList source.definitionSourceNames,
        Set.null (Set.fromList (definitionNameOccKeys definitionName) `Set.intersection` parsedMemberOccKeys),
        GHC.nameSrcSpan definitionName `GHC.isSubspanOf` parsedMember.parsedMemberSpan
      ]

    parsedMemberOccKeys =
      Set.fromList (map parsedMemberOccKey parsedMembers)

    namesWithinMemberSpan candidateNames memberSpan =
      [ candidateName
      | candidateName <- candidateNames,
        GHC.nameSrcSpan candidateName `GHC.isSubspanOf` memberSpan
      ]

collectDefinitionOccurrenceFacts ::
  GHC.Module ->
  DeclarationSpans ->
  DefinitionMemberIndex ->
  Map.Map SpanKey ParsedOccurrenceSyntax ->
  Map.Map ImportId ImportCandidate ->
  [MinimalTypedOccurrence] ->
  [DefinitionOccurrenceFact]
collectDefinitionOccurrenceFacts definingModule spans memberIndex parsedOccurrenceSyntaxBySpan importCandidatesById typedOccurrences =
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
            occurrenceFactOwners =
              chooseOccurrenceOwners
                memberIndex
                occurrence.typedOccurrenceParent
                occurrence.typedOccurrenceSpan,
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

chooseOccurrenceOwners ::
  DefinitionMemberIndex ->
  Maybe GHC.Name ->
  GHC.SrcSpan ->
  Set.Set GHC.Name
chooseOccurrenceOwners memberIndex maybeParent occurrenceSpan
  | not (Set.null narrowestOwners) =
      if not (Set.null narrowedParentOwners)
        then narrowedParentOwners
        else narrowestOwners
  | not (Set.null rootParentOwners) =
      rootParentOwners
  | otherwise =
      memberIndex.rootMemberNames
  where
    allDeclarationNames =
      memberIndex.rootMemberNames
        <> Set.fromList (map memberName memberIndex.scopedMembers)

    parentOwners =
      Set.fromList
        [ parentName
        | parentName <- maybeToList maybeParent,
          parentName `Set.member` allDeclarationNames
        ]

    rootParentOwners =
      Set.intersection parentOwners memberIndex.rootMemberNames

    containingMembers =
      [ member
      | member <- memberIndex.scopedMembers,
        occurrenceSpan `GHC.isSubspanOf` member.memberSpan
      ]

    narrowestSpanSize =
      minimumMaybe (map (memberSpanSize . memberSpan) containingMembers)

    narrowestOwners =
      case narrowestSpanSize of
        Nothing ->
          Set.empty
        Just minSize ->
          Set.fromList
            [ member.memberName
            | member <- containingMembers,
              memberSpanSize member.memberSpan == minSize
            ]

    narrowedParentOwners =
      Set.intersection parentOwners narrowestOwners

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe values = Just (minimum values)

memberSpanSize :: GHC.SrcSpan -> Int
memberSpanSize = \case
  GHC.RealSrcSpan realSpan _ ->
    let lineSpan = GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan
        colSpan =
          if lineSpan == 0
            then GHC.srcSpanEndCol realSpan - GHC.srcSpanStartCol realSpan
            else GHC.srcSpanEndCol realSpan
     in lineSpan * 10_000 + colSpan
  GHC.UnhelpfulSpan {} ->
    maxBound

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
        && left.occurrenceFactOwners == right.occurrenceFactOwners
        && left.occurrenceFactParent == right.occurrenceFactParent
        && left.occurrenceFactImportCandidates == right.occurrenceFactImportCandidates

dedupeParsedDefinitionMembers :: [ParsedDefinitionMember] -> [ParsedDefinitionMember]
dedupeParsedDefinitionMembers =
  List.nubBy sameMember
  where
    sameMember left right =
      left.parsedMemberOccKey == right.parsedMemberOccKey
        && left.parsedMemberSpan == right.parsedMemberSpan

dedupeDefinitionMembersByNameSpan :: [DefinitionMember] -> [DefinitionMember]
dedupeDefinitionMembersByNameSpan =
  List.nubBy sameMember
  where
    sameMember left right =
      left.memberName == right.memberName
        && left.memberSpan == right.memberSpan

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
