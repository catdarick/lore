module Lore.Internal.Definition.Analysis
  ( collectParsedOccurrenceNames,
    buildParsedModuleSummary,
    buildMinimalTypedModuleFacts,
    buildProcessedTypedDefinitionFacts,
    buildReferenceModuleAnalysis,
    mergeReferenceModuleAnalysisWithCoreFacts,
    buildUsedInstancesByBinder,
    normalizeImportItems,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data, Typeable, cast, gmapQ)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Data.Bag as Bag
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import qualified GHC.Types.TypeEnv as GHC.TypeEnv
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionAnalysis (..), DefinitionSlice (..), ImportQualifiedStyle (..), MinimalCoreModuleFacts (..), MinimalTypedImport (..), MinimalTypedModuleFacts (..), MinimalTypedOccurrence (..), ParsedDefinitionMatch (..), ParsedModuleSummary (..), ParsedOccurrenceSyntax (..), ProcessedTypedDefinitionFacts (..), ReferenceModuleAnalysis (..), RequiredImport (..), RequiredImportItem (..))

data ReferencedOccurrence = ReferencedOccurrence
  { occurrenceName :: GHC.Name,
    occurrenceSpan :: GHC.SrcSpan,
    occurrenceUsageSpans :: [GHC.SrcSpan],
    occurrenceSectionSpans :: [GHC.SrcSpan],
    occurrenceParent :: Maybe GHC.Name,
    occurrenceSyntax :: ParsedOccurrenceSyntax,
    occurrenceCandidates :: [Int]
  }

data SourceImport = SourceImport
  { sourceImportId :: Int,
    sourceImport :: RequiredImport
  }

buildParsedModuleSummary :: GHC.ParsedSource -> ParsedModuleSummary
buildParsedModuleSummary parsedSource =
  ParsedModuleSummary
    { parsedModuleOccurrenceNames =
        collectParsedOccurrenceNames parsedSource,
      parsedModuleDefinitions =
        [ parsedMatch
        | decl <- decls,
          name <- take 1 (collectTyped decl :: [GHC.LocatedN GHC.RdrName]),
          let parsedMatch =
                ParsedDefinitionMatch
                  { parsedDefinitionSpans =
                      DeclarationSpans
                        { declarationSpan = GHC.getLocA decl,
                          signatureSpan = GHC.getLocA <$> findSignatureDeclaration (GHC.rdrNameOcc (GHC.unLoc name)) decls
                        },
                    parsedOccurrenceSyntaxes =
                      collectOccurrenceSyntax
                        ( GHC.getLocA decl
                            : maybeToList (GHC.getLocA <$> findSignatureDeclaration (GHC.rdrNameOcc (GHC.unLoc name)) decls)
                        )
                        parsedSource
                  }
        ]
    }
  where
    decls =
      GHC.hsmodDecls $
        GHC.unLoc parsedSource

collectParsedOccurrenceNames :: GHC.ParsedSource -> Set.Set T.Text
collectParsedOccurrenceNames parsedSource =
  Set.fromList
    [ T.pack (GHC.occNameString (GHC.rdrNameOcc (GHC.unLoc locatedName)))
    | locatedName <- collectTyped parsedSource :: [GHC.LocatedN GHC.RdrName]
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

buildProcessedTypedDefinitionFacts ::
  GHC.Module ->
  ParsedModuleSummary ->
  MinimalTypedModuleFacts ->
  Map.Map GHC.Name ProcessedTypedDefinitionFacts
buildProcessedTypedDefinitionFacts definingModule parsedSummary typedModuleFacts =
  Map.fromList $
    [ (definitionName, processedFacts)
    | definitionName <- typedModuleFacts.typedDefinitionNames,
      Just processedFacts <- [buildProcessedTypedDefinitionFactsForName definingModule parsedSummary typedModuleFacts definitionName]
    ]

buildReferenceModuleAnalysis ::
  GHC.Module ->
  ParsedModuleSummary ->
  Map.Map GHC.Name ProcessedTypedDefinitionFacts ->
  ReferenceModuleAnalysis
buildReferenceModuleAnalysis definingModule parsedSummary typedFactsByDefinition =
  ReferenceModuleAnalysis
    { referenceModuleDefinitions =
        Map.fromList
          [ (definitionName, buildDefinitionAnalysis definingModule definitionName parsedSummary typedFacts)
          | (definitionName, typedFacts) <- Map.toList typedFactsByDefinition
          ]
    }

mergeReferenceModuleAnalysisWithCoreFacts ::
  MinimalCoreModuleFacts ->
  ReferenceModuleAnalysis ->
  ReferenceModuleAnalysis
mergeReferenceModuleAnalysisWithCoreFacts coreFacts moduleAnalysis =
  moduleAnalysis
    { referenceModuleDefinitions =
        Map.mapWithKey augmentDefinition moduleAnalysis.referenceModuleDefinitions
    }
  where
    augmentDefinition targetName = fmap \definitionAnalysis ->
      definitionAnalysis
        { analysisUsedInstances =
            dedupeNames
              ( definitionAnalysis.analysisUsedInstances
                  <> Map.findWithDefault [] targetName coreFacts.coreUsedInstancesByBinder
              )
        }

buildUsedInstancesByBinder :: [GHC.CoreBind] -> Map.Map GHC.Name [GHC.Name]
buildUsedInstancesByBinder coreBinds =
  Map.fromListWith (<>) $ concatMap bindingEntries coreBinds
  where
    bindingEntries = \case
      GHC.NonRec binder rhs ->
        [(GHC.getName binder, dedupeNames (collectExprUsedInstanceNames rhs))]
      GHC.Rec bindings ->
        [ (GHC.getName binder, dedupeNames (collectExprUsedInstanceNames rhs))
        | (binder, rhs) <- bindings
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
      { typedImportId = importId,
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
              dedupeIds
                [ importId
                | importSpec <- Bag.bagToList (GHC.gre_imp gre),
                  Just importId <- [findImportId (GHC.is_dloc (GHC.is_decl importSpec))]
                ]
          }

    findImportId importSpan =
      fst <$> List.find ((== importSpan) . snd) importDeclSpans

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

buildProcessedTypedDefinitionFactsForName ::
  GHC.Module ->
  ParsedModuleSummary ->
  MinimalTypedModuleFacts ->
  GHC.Name ->
  Maybe ProcessedTypedDefinitionFacts
buildProcessedTypedDefinitionFactsForName definingModule parsedSummary typedModuleFacts target =
  case findDefinitionMatch target parsedSummary of
    Nothing ->
      Nothing
    Just ParsedDefinitionMatch {parsedDefinitionSpans, parsedOccurrenceSyntaxes} ->
      let sourceImports =
            map minimalImportToSourceImport typedModuleFacts.typedSourceImports
          occurrences =
            collectReferencedOccurrences
              definingModule
              parsedDefinitionSpans
              parsedOccurrenceSyntaxes
              sourceImports
              typedModuleFacts.typedOccurrences
       in Just
            ProcessedTypedDefinitionFacts
              { processedRequiredImports = buildImports sourceImports occurrences,
                processedReferences = collectReferencedNames target parsedDefinitionSpans occurrences,
                processedReferenceSpans = collectReferenceSpans occurrences,
                processedReferenceUsageSpans = collectReferenceUsageSpans occurrences,
                processedReferenceSectionSpans = collectReferenceSectionSpans occurrences
              }
  where
    minimalImportToSourceImport minimalImport =
      SourceImport
        { sourceImportId = minimalImport.typedImportId,
          sourceImport =
            RequiredImport
              { importKey = minimalImport.typedImportId,
                importModule = minimalImport.typedImportModule,
                importPackageQualifier = minimalImport.typedImportPackageQualifier,
                importSource = minimalImport.typedImportSource,
                importQualifiedStyle = minimalImport.typedImportQualifiedStyle,
                importAlias = minimalImport.typedImportAlias,
                importOriginallyExplicit = minimalImport.typedImportOriginallyExplicit,
                importItems = []
              }
        }

buildDefinitionAnalysis ::
  GHC.Module ->
  GHC.Name ->
  ParsedModuleSummary ->
  ProcessedTypedDefinitionFacts ->
  Maybe DefinitionAnalysis
buildDefinitionAnalysis definingModule target parsedSummary typedFacts =
  case findDefinitionMatch target parsedSummary of
    Nothing ->
      Nothing
    Just ParsedDefinitionMatch {parsedDefinitionSpans} ->
      Just
        DefinitionAnalysis
          { analysisSlice =
              DefinitionSlice
                { definitionModule = definingModule,
                  declarationSpans = [parsedDefinitionSpans],
                  requiredImports = typedFacts.processedRequiredImports
                },
            analysisReferences = typedFacts.processedReferences,
            analysisUsedInstances = [],
            analysisReferenceSpans = typedFacts.processedReferenceSpans,
            analysisReferenceUsageSpans = typedFacts.processedReferenceUsageSpans,
            analysisReferenceSectionSpans = typedFacts.processedReferenceSectionSpans
          }

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

findDefinitionMatch :: GHC.Name -> ParsedModuleSummary -> Maybe ParsedDefinitionMatch
findDefinitionMatch target parsedSummary =
  List.find matchesTarget parsedSummary.parsedModuleDefinitions
  where
    matchesTarget parsedMatch =
      GHC.nameSrcSpan target `GHC.isSubspanOf` parsedMatch.parsedDefinitionSpans.declarationSpan

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

collectReferencedOccurrences ::
  GHC.Module ->
  DeclarationSpans ->
  [(GHC.SrcSpan, ParsedOccurrenceSyntax)] ->
  [SourceImport] ->
  [MinimalTypedOccurrence] ->
  [ReferencedOccurrence]
collectReferencedOccurrences definingModule spans parsedOccurrenceSyntaxes sourceImports typedOccurrences =
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
            fromMaybe (ParsedOccurrenceSyntax Nothing [] []) $
              lookup occurrence.typedOccurrenceSpan parsedOccurrenceSyntaxes
      guardReference definingModule spans occurrenceName $
        ReferencedOccurrence
          { occurrenceName,
            occurrenceSpan = occurrence.typedOccurrenceSpan,
            occurrenceUsageSpans = syntax.parsedSyntaxUsageSpans,
            occurrenceSectionSpans = syntax.parsedSyntaxSectionSpans,
            occurrenceParent = occurrence.typedOccurrenceParent,
            occurrenceSyntax = syntax,
            occurrenceCandidates =
              dedupeIds
                [ importId
                | importId <- occurrence.typedOccurrenceCandidates,
                  supportsOccurrence syntax importId
                ]
          }

    supportsOccurrence syntax importId =
      case List.find ((== importId) . sourceImportId) sourceImports of
        Nothing -> False
        Just sourceImport -> supportsOccurrenceSyntax syntax sourceImport.sourceImport

collectOccurrenceSyntax ::
  [GHC.SrcSpan] ->
  GHC.ParsedSource ->
  [(GHC.SrcSpan, ParsedOccurrenceSyntax)]
collectOccurrenceSyntax targetSpans parsedSource =
  [ (span', syntaxForOccurrence span' (GHC.unLoc locatedName))
  | locatedName <- collectTyped parsedSource :: [GHC.LocatedN GHC.RdrName],
    let span' = locatedSpan locatedName,
    spanWithin targetSpans span'
  ]
  where
    usageSpanMap = collectOccurrenceUsageSpans targetSpans parsedSource

    syntaxForOccurrence span' rdrName =
      ParsedOccurrenceSyntax
        { parsedSyntaxQualifier = fmap fst (GHC.isQual_maybe rdrName),
          parsedSyntaxUsageSpans = Map.findWithDefault [] (show span') usageSpanMap,
          parsedSyntaxSectionSpans = Map.findWithDefault [] (show span') sectionSpanMap
        }
    sectionSpanMap = collectOccurrenceSectionSpans targetSpans parsedSource

collectTyped :: forall b a. (Typeable b, Data a) => a -> [b]
collectTyped = go
  where
    go :: forall x. (Data x) => x -> [b]
    go value =
      maybeToList (cast value) <> concat (gmapQ go value)

locatedSpan :: GHC.LocatedN a -> GHC.SrcSpan
locatedSpan =
  GHC.locA . GHC.getLoc

locatedASpan :: GHC.LocatedA a -> GHC.SrcSpan
locatedASpan = GHC.getLocA

supportsOccurrenceSyntax :: ParsedOccurrenceSyntax -> RequiredImport -> Bool
supportsOccurrenceSyntax ParsedOccurrenceSyntax {parsedSyntaxQualifier} requiredImport =
  case parsedSyntaxQualifier of
    Nothing ->
      requiredImport.importQualifiedStyle == NotQualified
    Just qualifier ->
      requiredImport.importAlias == Just qualifier

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
                  [(candidateId, 1 :: Int) | ref <- remaining, candidateId <- ref.occurrenceCandidates]
              bestImport =
                fst $
                  List.maximumBy (\a b -> compare (snd a) (snd b)) $
                    IntMap.toList counts
              chosen' = IntSet.insert bestImport chosen
           in go chosen' (filter (not . coveredBy chosen') remaining)

        coveredBy chosen ref =
          any (`IntSet.member` chosen) ref.occurrenceCandidates

    buildRequiredImport (importId, refs) = do
      sourceImport <- List.find ((== importId) . sourceImportId) sourceImports
      pure
        sourceImport.sourceImport
          { importItems = normalizeImportItems (concatMap occurrenceItems refs)
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

collectReferenceSpans :: [ReferencedOccurrence] -> Map.Map GHC.Name [GHC.SrcSpan]
collectReferenceSpans =
  Map.fromListWith (<>)
    . map (\occurrence -> (occurrence.occurrenceName, [occurrence.occurrenceSpan]))

collectReferenceUsageSpans :: [ReferencedOccurrence] -> Map.Map GHC.Name [GHC.SrcSpan]
collectReferenceUsageSpans =
  Map.map dedupeSrcSpans
    . Map.fromListWith (<>)
    . concatMap
      ( \occurrence ->
          [ (occurrence.occurrenceName, [usageSpan])
          | usageSpan <- occurrence.occurrenceUsageSpans
          ]
      )

collectReferenceSectionSpans :: [ReferencedOccurrence] -> Map.Map GHC.Name [GHC.SrcSpan]
collectReferenceSectionSpans =
  Map.map dedupeSrcSpans
    . Map.fromListWith (<>)
    . concatMap
      ( \occurrence ->
          [ (occurrence.occurrenceName, [sectionSpan])
          | sectionSpan <- occurrence.occurrenceSectionSpans
          ]
      )

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

pkgQualString :: GHC.PkgQual -> Maybe String
pkgQualString = \case
  GHC.NoPkgQual -> Nothing
  pkgQual -> Just (GHC.showSDocUnsafe (GHC.ppr pkgQual))

dedupeOccurrences :: [ReferencedOccurrence] -> [ReferencedOccurrence]
dedupeOccurrences =
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.occurrenceName == right.occurrenceName
        && left.occurrenceSpan == right.occurrenceSpan
        && left.occurrenceUsageSpans == right.occurrenceUsageSpans
        && left.occurrenceSectionSpans == right.occurrenceSectionSpans
        && left.occurrenceParent == right.occurrenceParent
        && left.occurrenceSyntax == right.occurrenceSyntax
        && left.occurrenceCandidates == right.occurrenceCandidates

dedupeMinimalTypedOccurrences :: [MinimalTypedOccurrence] -> [MinimalTypedOccurrence]
dedupeMinimalTypedOccurrences =
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.typedOccurrenceName == right.typedOccurrenceName
        && left.typedOccurrenceSpan == right.typedOccurrenceSpan
        && left.typedOccurrenceParent == right.typedOccurrenceParent
        && left.typedOccurrenceCandidates == right.typedOccurrenceCandidates

dedupeIds :: [Int] -> [Int]
dedupeIds =
  IntSet.toAscList . IntSet.fromList

dedupeNames :: [GHC.Name] -> [GHC.Name]
dedupeNames =
  Map.elems . Map.fromList . map (\name -> (GHC.occNameString (GHC.nameOccName name), name))

dedupeSrcSpans :: [GHC.SrcSpan] -> [GHC.SrcSpan]
dedupeSrcSpans =
  Map.elems . Map.fromList . map (\span' -> (show span', span'))

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans

data UsageKind
  = UsageKindRecord
  | UsageKindApplication
  deriving stock (Eq, Show)

data UsageSpanCandidate = UsageSpanCandidate
  { usageSpan :: GHC.SrcSpan,
    usageKind :: UsageKind
  }

collectOccurrenceUsageSpans :: [GHC.SrcSpan] -> GHC.ParsedSource -> Map.Map String [GHC.SrcSpan]
collectOccurrenceUsageSpans targetSpans parsedSource =
  Map.map (map (.usageSpan) . dedupeUsageCandidates . sortOn compareKey) $
    Map.fromListWith (<>) (concatMap expressionUsageEntries expressions)
  where
    expressions = collectTyped parsedSource :: [GHC.LocatedA (GHC.HsExpr GHC.GhcPs)]

    expressionUsageEntries expression@(GHC.L _ expr)
      | not (spanWithin targetSpans expressionSpan) = []
      | otherwise =
          case usageKindForExpression expr of
            Nothing -> []
            Just usageKind ->
              [ (show occurrenceSpan, [UsageSpanCandidate {usageSpan = expressionSpan, usageKind}])
              | occurrenceSpan <- expressionOccurrences expr,
                spanWithin targetSpans occurrenceSpan,
                usageSpanImprovesOccurrence occurrenceSpan expressionSpan
              ]
      where
        expressionSpan = locatedASpan expression

    compareKey candidate = usageCandidateRank candidate

dedupeUsageCandidates :: [UsageSpanCandidate] -> [UsageSpanCandidate]
dedupeUsageCandidates =
  Map.elems . Map.fromList . map (\candidate -> (show candidate.usageSpan, candidate))

usageCandidateRank :: UsageSpanCandidate -> (Int, Int, Int, Int)
usageCandidateRank UsageSpanCandidate {usageSpan, usageKind} =
  case GHC.srcSpanToRealSrcSpan usageSpan of
    Nothing -> (0, 0, 0, 0)
    Just realSpan ->
      ( usageKindPriority usageKind,
        if GHC.srcSpanEndLine realSpan > GHC.srcSpanStartLine realSpan then 1 else 0,
        usageKindLinePreference usageKind (GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan),
        usageKindColumnPreference usageKind (GHC.srcSpanEndCol realSpan - GHC.srcSpanStartCol realSpan)
      )

usageKindPriority :: UsageKind -> Int
usageKindPriority = \case
  UsageKindRecord -> 3
  UsageKindApplication -> 2

usageKindLinePreference :: UsageKind -> Int -> Int
usageKindLinePreference usageKind lineSpan =
  case usageKind of
    UsageKindApplication -> lineSpan
    _ -> negate lineSpan

usageKindColumnPreference :: UsageKind -> Int -> Int
usageKindColumnPreference usageKind columnSpan =
  case usageKind of
    UsageKindApplication -> columnSpan
    _ -> negate columnSpan

usageKindForExpression :: GHC.HsExpr GHC.GhcPs -> Maybe UsageKind
usageKindForExpression = \case
  GHC.RecordCon {} -> Just UsageKindRecord
  GHC.RecordUpd {} -> Just UsageKindRecord
  GHC.HsApp {} -> Just UsageKindApplication
  GHC.HsAppType {} -> Just UsageKindApplication
  GHC.OpApp {} -> Just UsageKindApplication
  GHC.NegApp {} -> Just UsageKindApplication
  GHC.HsPar _ _ expression _ -> usageKindForExpression (GHC.unLoc expression)
  _ -> Nothing

collectOccurrenceSectionSpans :: [GHC.SrcSpan] -> GHC.ParsedSource -> Map.Map String [GHC.SrcSpan]
collectOccurrenceSectionSpans targetSpans parsedSource =
  Map.map dedupeSrcSpans $ Map.fromListWith (<>) (concatMap sectionEntries sectionSpans)
  where
    sectionSpans = collectParsedSectionSpans parsedSource
    occurrenceSpans =
      [ locatedSpan locatedName
      | locatedName <- collectTyped parsedSource :: [GHC.LocatedN GHC.RdrName]
      ]

    sectionEntries sectionSpan =
      [ (show occurrenceSpan, [sectionSpan])
      | occurrenceSpan <- occurrenceSpans,
        occurrenceSpan `GHC.isSubspanOf` sectionSpan,
        spanWithin targetSpans occurrenceSpan
      ]

collectParsedSectionSpans :: GHC.ParsedSource -> [GHC.SrcSpan]
collectParsedSectionSpans parsedSource =
  map locatedASpan (collectTyped parsedSource :: [GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)])
    <> map grhsSpan (collectTyped parsedSource :: [GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)])
    <> map locatedASpan (collectTyped parsedSource :: [GHC.LStmt GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)])
    <> map locatedASpan (collectTyped parsedSource :: [GHC.LHsBind GHC.GhcPs])

grhsSpan :: GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GHC.SrcSpan
grhsSpan = GHC.locA . GHC.getLoc

usageSpanImprovesOccurrence :: GHC.SrcSpan -> GHC.SrcSpan -> Bool
usageSpanImprovesOccurrence occurrenceSpan usageSpan =
  case (GHC.srcSpanToRealSrcSpan occurrenceSpan, GHC.srcSpanToRealSrcSpan usageSpan) of
    (Just occurrenceRealSpan, Just usageRealSpan) ->
      GHC.srcSpanFile occurrenceRealSpan == GHC.srcSpanFile usageRealSpan
        && ( GHC.srcSpanStartLine usageRealSpan < GHC.srcSpanStartLine occurrenceRealSpan
               || GHC.srcSpanEndLine usageRealSpan > GHC.srcSpanEndLine occurrenceRealSpan
               || GHC.srcSpanStartCol usageRealSpan < GHC.srcSpanStartCol occurrenceRealSpan
               || GHC.srcSpanEndCol usageRealSpan > GHC.srcSpanEndCol occurrenceRealSpan
           )
    _ -> False

expressionOccurrences :: GHC.HsExpr GHC.GhcPs -> [GHC.SrcSpan]
expressionOccurrences =
  map locatedSpan . (collectTyped :: GHC.HsExpr GHC.GhcPs -> [GHC.LocatedN GHC.RdrName])
