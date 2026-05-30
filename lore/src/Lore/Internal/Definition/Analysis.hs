{-# OPTIONS_GHC -Wno-identities #-}

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
    buildEvidenceDependenciesByBinder,
    buildSemanticDependenciesByBinder,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Data (Data, Typeable, cast, gmapQ)
import Data.Foldable (foldl')
import qualified Data.Graph as Graph
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC
import qualified GHC.Core.Coercion.Axiom as GHCAxiom
import qualified GHC.Core.FamInstEnv as GHC.FamInst
import qualified GHC.Core.InstEnv as GHC.InstEnv
import qualified GHC.Core.TyCo.FVs as GHC.TyCoFVs
import qualified GHC.Core.TyCo.Rep as GHCTyCo
import qualified GHC.Data.Strict as Strict
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import qualified GHC.Types.Avail as GHC
import qualified GHC.Types.FieldLabel as GHC.FieldLabel
import qualified GHC.Types.Unique as GHCUnique
import qualified GHC.Types.Unique.Set as GHC.UniqueSet
import Lore.Internal.Definition.SourceTree (collectModuleSourceRegionCandidates)
import Lore.Internal.Definition.Types
import Lore.Internal.Ghc.AvailInfo (availInfoGreNames, availInfoNamesWithFields, fieldLabelAliasText, greNameFieldAliasText)
import Lore.Internal.List (minimumMaybe)

buildParsedModuleFacts :: GHC.Module -> GHC.ParsedSource -> ParsedModuleFacts
buildParsedModuleFacts definingModule parsedSource =
  ParsedModuleFacts
    { parsedOccKeys = collectParsedOccurrenceNames parsedSource,
      parsedDeclarationsById = Map.fromList declarationEntries,
      parsedDefinitionMembersById = Map.fromListWith (<>) definitionMemberEntries,
      parsedRegionCandidates = collectModuleSourceRegionCandidates parsedSource
    }
  where
    decls = GHC.hsmodDecls $ GHC.unLoc parsedSource

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

collectParsedOccurrenceNames :: GHC.ParsedSource -> Set.Set OccKey
collectParsedOccurrenceNames parsedSource =
  Set.fromList (rdrNameKeys <> dotFieldKeys)
  where
    rdrNameKeys =
      [ rdrNameOccKey (GHC.unLoc locatedName)
      | locatedName <- collectLocatedRdrNames parsedSource
      ]

    dotFieldKeys =
      [ occNameKey (GHC.rdrNameOcc (dotFieldLabelRdrNamePs dotFieldOccurrence))
      | dotFieldOccurrence <- collectTyped parsedSource :: [GHC.DotFieldOcc GHC.GhcPs]
      ]

buildMinimalTypedModuleFacts ::
  GHC.Module ->
  GHC.Tc.TcGblEnv ->
  MinimalTypedModuleFacts
buildMinimalTypedModuleFacts definingModule tcg =
  let familyInstanceNames =
        collectFamilyInstanceNames definingModule tcg
      instanceNames =
        collectClassInstanceNames definingModule tcg <> familyInstanceNames
   in MinimalTypedModuleFacts
        { typedDefinitionNames = collectDefinitionCandidateNames definingModule tcg,
          typedInstanceNames = instanceNames,
          typedInstanceHeadTypeNamesByInstance = collectInstanceHeadTypeNamesByInstance definingModule tcg,
          typedDefinitionOccAliases = collectDefinitionOccAliases definingModule tcg,
          typedExportedNames = collectExportedNames definingModule tcg,
          typedExportedOccAliases = collectExportedOccAliases definingModule tcg,
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
  MinimalTypedModuleFacts ->
  DefinitionBindings ->
  Map.Map DefinitionId DefinitionMemberIndex ->
  Map.Map DefinitionId [DefinitionOccurrenceFact]
buildDefinitionOccurrences definingModule typedModuleFacts bindings memberIndexesById =
  Map.map mkOccurrences bindings.bindingDefinitionsById
  where
    mkOccurrences source =
      let memberIndex =
            memberIndexesById Map.! source.definitionSourceId
       in collectDefinitionOccurrenceFacts
            definingModule
            source.definitionSourceSpans
            memberIndex
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
    coreEvidenceDependenciesByBinder =
      maybe Map.empty (.coreEvidenceDependenciesByBinder) maybeCoreFacts

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
                instanceName <- Map.findWithDefault [] binderName coreEvidenceDependenciesByBinder
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
      dependenciesById = buildDependencies bindings memberIndexesById occurrencesById maybeCoreFacts
    }
  where
    bindings =
      buildDefinitionBindings definingModule parsedFacts typedModuleFacts

    memberIndexesById =
      buildDefinitionMemberIndexes parsedFacts typedModuleFacts bindings

    occurrencesById =
      buildDefinitionOccurrences definingModule typedModuleFacts bindings memberIndexesById

buildEvidenceDependenciesByBinder ::
  Set.Set GHC.Name ->
  [GHC.CoreBind] ->
  Map.Map GHC.Name [GHC.Name]
buildEvidenceDependenciesByBinder interestingBinders coreBinds =
  Map.fromListWith (<>) $
    concatMap bindingEntries coreBinds
  where
    keepEntry binderName evidenceDependencies =
      [ (binderName, evidenceDependencies)
      | Set.member binderName interestingBinders,
        not (null evidenceDependencies)
      ]

    bindingEntries = \case
      GHC.NonRec binder rhs ->
        let evidenceDependencies =
              dedupeSemanticNamesByUnique (collectDirectEvidenceDependenciesInExpr rhs)
         in keepEntry (GHC.getName binder) evidenceDependencies
      GHC.Rec pairs ->
        concat
          [ keepEntry (GHC.getName binder) evidenceDependencies
          | (binder, rhs) <- pairs,
            let evidenceDependencies =
                  dedupeSemanticNamesByUnique (collectDirectEvidenceDependenciesInExpr rhs)
          ]

buildSemanticDependenciesByBinder ::
  Set.Set GHC.Name ->
  Set.Set GHC.Name ->
  [GHC.CoreBind] ->
  Map.Map GHC.Name [GHC.Name]
buildSemanticDependenciesByBinder interestingBinders interestingDependencyNames coreBinds =
  Map.fromList
    [ (binderName, semanticDependencies)
    | (binderKey, (binderName, _)) <- IntMap.toList topLevelBindingsByKey,
      IntSet.member binderKey interestingBinderKeys,
      Just semanticDependencies <- [IntMap.lookup binderKey semanticDependenciesByKey],
      not (null semanticDependencies)
    ]
  where
    interestingBinderKeys =
      IntSet.fromList (map nameUniqueKey (Set.toList interestingBinders))

    interestingDependencyKeys =
      IntSet.fromList (map nameUniqueKey (Set.toList interestingDependencyNames))

    topLevelBindingsByKey =
      collectTopLevelBindingsByKey coreBinds

    topLevelBindingKeys =
      IntMap.keysSet topLevelBindingsByKey

    directDependenciesByKey =
      IntMap.map
        (collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys . snd)
        topLevelBindingsByKey

    semanticDependenciesByKey =
      buildTransitiveCoreSemanticDependencies directDependenciesByKey

buildTransitiveCoreSemanticDependencies ::
  IntMap.IntMap CoreDirectDependencies ->
  IntMap.IntMap [GHC.Name]
buildTransitiveCoreSemanticDependencies directDependenciesByKey =
  IntMap.fromList
    [ (binderKey, componentSemanticDependencies)
    | (componentId, componentKeys) <- IntMap.toList componentKeysByComponent,
      let componentSemanticDependencies =
            IntMap.findWithDefault [] componentId semanticDependenciesByComponent,
      binderKey <- IntSet.toList componentKeys
    ]
  where
    componentKeysByComponent =
      IntMap.fromList
        [ (componentId, IntSet.fromList componentKeys)
        | (componentId, componentKeys) <- zip [0 ..] (map sccKeys dependencySccs)
        ]

    componentByKey =
      IntMap.fromList
        [ (binderKey, componentId)
        | (componentId, componentKeys) <- IntMap.toList componentKeysByComponent,
          binderKey <- IntSet.toList componentKeys
        ]

    dependencySccs =
      Graph.stronglyConnComp
        [ (binderKey, binderKey, coreDirectTopLevelBinderKeys directDependencies)
        | (binderKey, directDependencies) <- IntMap.toList directDependenciesByKey
        ]

    componentDirectSemanticNames =
      IntMap.map
        ( \componentKeys ->
            concat
              [ coreDirectSemanticNames directDependencies
              | binderKey <- IntSet.toList componentKeys,
                Just directDependencies <- [IntMap.lookup binderKey directDependenciesByKey]
              ]
        )
        componentKeysByComponent

    componentDependencyComponents =
      IntMap.mapWithKey
        ( \componentId componentKeys ->
            IntSet.fromList
              [ dependencyComponentId
              | binderKey <- IntSet.toList componentKeys,
                Just directDependencies <- [IntMap.lookup binderKey directDependenciesByKey],
                dependencyKey <- coreDirectTopLevelBinderKeys directDependencies,
                Just dependencyComponentId <- [IntMap.lookup dependencyKey componentByKey],
                dependencyComponentId /= componentId
              ]
        )
        componentKeysByComponent

    semanticDependenciesByComponent =
      foldl'
        addComponentSemanticDependencies
        IntMap.empty
        (IntMap.toAscList componentDirectSemanticNames)

    addComponentSemanticDependencies accumulatedDependenciesByComponent (componentId, directSemanticNames) =
      IntMap.insert
        componentId
        ( dedupeSemanticNamesByUnique $
            directSemanticNames
              <> concat
                [ IntMap.findWithDefault [] dependencyComponentId accumulatedDependenciesByComponent
                | dependencyComponentId <-
                    IntSet.toList $
                      IntMap.findWithDefault IntSet.empty componentId componentDependencyComponents
                ]
        )
        accumulatedDependenciesByComponent

sccKeys :: Graph.SCC Int -> [Int]
sccKeys = \case
  Graph.AcyclicSCC key ->
    [key]
  Graph.CyclicSCC keys ->
    keys

data CoreDirectDependencies = CoreDirectDependencies
  { coreDirectSemanticNames :: [GHC.Name],
    coreDirectTopLevelBinderKeys :: [Int]
  }

emptyCoreDirectDependencies :: CoreDirectDependencies
emptyCoreDirectDependencies =
  CoreDirectDependencies
    { coreDirectSemanticNames = [],
      coreDirectTopLevelBinderKeys = []
    }

appendCoreDirectDependencies :: CoreDirectDependencies -> CoreDirectDependencies -> CoreDirectDependencies
appendCoreDirectDependencies left right =
  CoreDirectDependencies
    { coreDirectSemanticNames =
        left.coreDirectSemanticNames <> right.coreDirectSemanticNames,
      coreDirectTopLevelBinderKeys =
        left.coreDirectTopLevelBinderKeys <> right.coreDirectTopLevelBinderKeys
    }

concatCoreDirectDependencies :: [CoreDirectDependencies] -> CoreDirectDependencies
concatCoreDirectDependencies =
  foldr appendCoreDirectDependencies emptyCoreDirectDependencies

collectTopLevelBindingsByKey :: [GHC.CoreBind] -> IntMap.IntMap (GHC.Name, GHC.CoreExpr)
collectTopLevelBindingsByKey =
  IntMap.fromList . concatMap toEntries
  where
    toEntries = \case
      GHC.NonRec binder rhs ->
        let binderName = GHC.getName binder
         in [(nameUniqueKey binderName, (binderName, rhs))]
      GHC.Rec pairs ->
        [ let binderName = GHC.getName binder
           in (nameUniqueKey binderName, (binderName, rhs))
        | (binder, rhs) <- pairs
        ]

collectDefinitionCandidateNames :: GHC.Module -> GHC.Tc.TcGblEnv -> [GHC.Name]
collectDefinitionCandidateNames homeModule tcg =
  nubOrd (localGreNames <> fieldSelectorNames <> instanceNames)
  where
    belongsToModule name =
      GHC.nameModule_maybe name == Just homeModule

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
      collectClassInstanceNames homeModule tcg <> collectFamilyInstanceNames homeModule tcg

collectClassInstanceNames :: GHC.Module -> GHC.Tc.TcGblEnv -> [GHC.Name]
collectClassInstanceNames homeModule tcg =
  filter belongsToModule (map GHC.getName (GHC.Tc.tcg_insts tcg))
  where
    belongsToModule name =
      GHC.nameModule_maybe name == Just homeModule

collectFamilyInstanceNames :: GHC.Module -> GHC.Tc.TcGblEnv -> [GHC.Name]
collectFamilyInstanceNames homeModule tcg =
  filter belongsToModule (map GHC.getName (GHC.Tc.tcg_fam_insts tcg))
  where
    belongsToModule name =
      GHC.nameModule_maybe name == Just homeModule

collectInstanceHeadTypeNamesByInstance ::
  GHC.Module ->
  GHC.Tc.TcGblEnv ->
  Map.Map GHC.Name (Set.Set GHC.Name)
collectInstanceHeadTypeNamesByInstance homeModule tcg =
  Map.fromListWith (<>) (classInstanceHeadEntries <> familyInstanceHeadEntries)
  where
    classInstanceHeadEntries =
      [ (instanceName, collectHeadTypeNames (GHC.InstEnv.is_tys classInstance))
      | classInstance <- GHC.Tc.tcg_insts tcg,
        let instanceName = GHC.getName (GHC.InstEnv.instanceDFunId classInstance),
        belongsToModule instanceName
      ]

    familyInstanceHeadEntries =
      [ (instanceName, collectHeadTypeNames (GHC.FamInst.fi_tys familyInstance))
      | familyInstance <- GHC.Tc.tcg_fam_insts tcg,
        let instanceName = GHC.getName familyInstance,
        belongsToModule instanceName
      ]

    belongsToModule name =
      GHC.nameModule_maybe name == Just homeModule

    collectHeadTypeNames instanceHeadTypes =
      Set.fromList
        [ GHC.getName tyCon
        | instanceHeadType <- instanceHeadTypes,
          tyCon <- GHC.UniqueSet.nonDetEltsUniqSet (GHC.TyCoFVs.tyConsOfType instanceHeadType)
        ]

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

collectExportedNames :: GHC.Module -> GHC.Tc.TcGblEnv -> [GHC.Name]
collectExportedNames homeModule tcg =
  nubOrd
    [ name
    | availInfo <- GHC.Tc.tcg_exports tcg,
      name <- availInfoNamesWithFields availInfo,
      GHC.nameModule_maybe name == Just homeModule
    ]

collectExportedOccAliases :: GHC.Module -> GHC.Tc.TcGblEnv -> Map.Map GHC.Name (Set.Set Text)
collectExportedOccAliases homeModule tcg =
  Map.fromListWith
    Set.union
    [ (name, Set.singleton aliasText)
    | availInfo <- GHC.Tc.tcg_exports tcg,
      greName <- availInfoGreNames availInfo,
      name <- [GHC.greNamePrintableName greName],
      GHC.nameModule_maybe name == Just homeModule,
      Just aliasText <- [greNameFieldAliasText greName]
    ]

collectMinimalTypedOccurrences :: GHC.Tc.TcGblEnv -> [MinimalTypedOccurrence]
collectMinimalTypedOccurrences tcg =
  case GHC.Tc.tcg_rn_decls tcg of
    Nothing -> []
    Just renamedGroup ->
      dedupeMinimalTypedOccurrences . concatMap toMinimalTypedOccurrences $ collectOccurrenceSeeds renamedGroup
  where
    globalRdrEnv =
      GHC.Tc.tcg_rdr_env tcg

    collectOccurrenceSeeds renamedGroup =
      namedOccurrenceSeeds <> fieldOccurrenceSeeds <> dotFieldOccurrenceSeeds
      where
        namedOccurrenceSeeds =
          [ OccurrenceSeed
              { occurrenceSeedSpan = locatedSpan locatedName,
                occurrenceSeedGres =
                  maybeToList (GHC.lookupGRE_Name globalRdrEnv (GHC.unLoc locatedName))
              }
          | locatedName <- collectTyped renamedGroup :: [GHC.LocatedN GHC.Name]
          ]

        fieldOccurrenceSeeds =
          [ OccurrenceSeed
              { occurrenceSeedSpan = GHC.getLocA fieldOccurrence.foLabel,
                occurrenceSeedGres =
                  maybeToList $
                    List.find
                      (matchesFieldSelector (GHC.foExt fieldOccurrence))
                      (GHC.lookupGRE_RdrName (GHC.unLoc fieldOccurrence.foLabel) globalRdrEnv)
              }
          | fieldOccurrence <- collectTyped renamedGroup :: [GHC.FieldOcc GHC.GhcRn]
          ]

        dotFieldOccurrenceSeeds =
          [ OccurrenceSeed
              { occurrenceSeedSpan = GHC.getLocA dotFieldOccurrence.dfoLabel,
                occurrenceSeedGres =
                  filter
                    isDotFieldSelectorGre
                    (GHC.lookupGRE_RdrName (dotFieldLabelRdrNameRn dotFieldOccurrence) globalRdrEnv)
              }
          | dotFieldOccurrence <- collectTyped renamedGroup :: [GHC.DotFieldOcc GHC.GhcRn]
          ]

    toMinimalTypedOccurrences occurrenceSeed =
      [ MinimalTypedOccurrence
          { typedOccurrenceName = GHC.greNamePrintableName gre.gre_name,
            typedOccurrenceSpan = occurrenceSeed.occurrenceSeedSpan,
            typedOccurrenceParent = case GHC.gre_par gre of
              GHC.ParentIs parentName -> Just parentName
              GHC.NoParent -> Nothing
          }
      | gre <- occurrenceSeed.occurrenceSeedGres
      ]

    matchesFieldSelector selectorName gre =
      case gre.gre_name of
        GHC.NormalGreName name ->
          name == selectorName
        GHC.FieldGreName fieldLabel ->
          GHC.FieldLabel.flSelector fieldLabel == selectorName

    isDotFieldSelectorGre gre =
      case gre.gre_name of
        GHC.FieldGreName _ ->
          True
        GHC.NormalGreName _ ->
          False

data OccurrenceSeed = OccurrenceSeed
  { occurrenceSeedSpan :: !GHC.SrcSpan,
    occurrenceSeedGres :: ![GHC.GlobalRdrElt]
  }

collectDirectEvidenceDependenciesInExpr :: GHC.CoreExpr -> [GHC.Name]
collectDirectEvidenceDependenciesInExpr = \case
  GHC.Var variable ->
    [GHC.getName variable | GHC.isDFunId variable]
  GHC.Lit _ ->
    []
  GHC.App function argument ->
    collectDirectEvidenceDependenciesInExpr function <> collectDirectEvidenceDependenciesInExpr argument
  GHC.Lam _ body ->
    collectDirectEvidenceDependenciesInExpr body
  GHC.Let binding body ->
    collectDirectEvidenceDependenciesInBind binding <> collectDirectEvidenceDependenciesInExpr body
  GHC.Case scrutinee _ _ alternatives ->
    collectDirectEvidenceDependenciesInExpr scrutinee
      <> concatMap collectDirectEvidenceDependenciesInAlt alternatives
  GHC.Cast expression _ ->
    collectDirectEvidenceDependenciesInExpr expression
  GHC.Tick _ expression ->
    collectDirectEvidenceDependenciesInExpr expression
  GHC.Type _ ->
    []
  GHC.Coercion _ ->
    []

collectDirectEvidenceDependenciesInBind :: GHC.CoreBind -> [GHC.Name]
collectDirectEvidenceDependenciesInBind = \case
  GHC.NonRec _ rhs ->
    collectDirectEvidenceDependenciesInExpr rhs
  GHC.Rec bindings ->
    concatMap (collectDirectEvidenceDependenciesInExpr . snd) bindings

collectDirectEvidenceDependenciesInAlt :: GHC.CoreAlt -> [GHC.Name]
collectDirectEvidenceDependenciesInAlt (GHC.Alt _ _ expression) =
  collectDirectEvidenceDependenciesInExpr expression

collectDirectCoreDependenciesInExpr ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  GHC.CoreExpr ->
  CoreDirectDependencies
collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys = \case
  GHC.Var variable ->
    let variableName = GHC.getName variable
        variableDependencies =
          coreDirectDependenciesForName interestingDependencyKeys topLevelBindingKeys variableName
     in if GHC.isDFunId variable
          then
            variableDependencies
              { coreDirectSemanticNames =
                  variableName : variableDependencies.coreDirectSemanticNames
              }
          else variableDependencies
  GHC.Lit _ ->
    emptyCoreDirectDependencies
  GHC.App function argument ->
    collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys function
      `appendCoreDirectDependencies` collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys argument
  GHC.Lam _ body ->
    collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys body
  GHC.Let binding body ->
    collectDirectCoreDependenciesInBind interestingDependencyKeys topLevelBindingKeys binding
      `appendCoreDirectDependencies` collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys body
  GHC.Case scrutinee _ _ alternatives ->
    collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys scrutinee
      `appendCoreDirectDependencies` concatCoreDirectDependencies
        (map (collectDirectCoreDependenciesInAlt interestingDependencyKeys topLevelBindingKeys) alternatives)
  GHC.Cast expression coercion ->
    collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys expression
      `appendCoreDirectDependencies` collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion
  GHC.Tick _ expression ->
    collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys expression
  GHC.Type type_ ->
    collectDirectCoreDependenciesInType interestingDependencyKeys topLevelBindingKeys type_
  GHC.Coercion coercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion

collectDirectCoreDependenciesInType ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  GHC.Type ->
  CoreDirectDependencies
collectDirectCoreDependenciesInType interestingDependencyKeys topLevelBindingKeys type_ =
  concatCoreDirectDependencies
    [ coreDirectDependenciesForName interestingDependencyKeys topLevelBindingKeys (GHC.getName tyCon)
    | tyCon <- GHC.UniqueSet.nonDetEltsUniqSet (GHC.TyCoFVs.tyConsOfType type_)
    ]

collectDirectCoreDependenciesInCoercion ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  GHCTyCo.Coercion ->
  CoreDirectDependencies
collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys = \case
  GHCTyCo.Refl type_ ->
    collectDirectCoreDependenciesInType interestingDependencyKeys topLevelBindingKeys type_
  GHCTyCo.GRefl _ type_ maybeCoercion ->
    collectDirectCoreDependenciesInType interestingDependencyKeys topLevelBindingKeys type_
      `appendCoreDirectDependencies` collectDirectCoreDependenciesInMCoercion interestingDependencyKeys topLevelBindingKeys maybeCoercion
  GHCTyCo.TyConAppCo _ _ coercions ->
    concatCoreDirectDependencies
      (map (collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys) coercions)
  GHCTyCo.AppCo coercionOne coercionTwo ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercionOne
      `appendCoreDirectDependencies` collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercionTwo
  GHCTyCo.ForAllCo _ kindCoercion coercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys kindCoercion
      `appendCoreDirectDependencies` collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion
  GHCTyCo.FunCo {GHCTyCo.fco_mult = multiplicityCoercion, GHCTyCo.fco_arg = argumentCoercion, GHCTyCo.fco_res = resultCoercion} ->
    concatCoreDirectDependencies
      [ collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys multiplicityCoercion,
        collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys argumentCoercion,
        collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys resultCoercion
      ]
  GHCTyCo.CoVarCo coercionVariable ->
    coreDirectDependenciesForName interestingDependencyKeys topLevelBindingKeys (GHC.getName coercionVariable)
  GHCTyCo.AxiomInstCo axiom _ coercions ->
    coreDirectDependenciesForName interestingDependencyKeys topLevelBindingKeys (GHCAxiom.coAxiomName axiom)
      `appendCoreDirectDependencies` concatCoreDirectDependencies
        (map (collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys) coercions)
  GHCTyCo.UnivCo _ _ _ _ ->
    emptyCoreDirectDependencies
  GHCTyCo.SymCo coercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion
  GHCTyCo.TransCo coercionOne coercionTwo ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercionOne
      `appendCoreDirectDependencies` collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercionTwo
  GHCTyCo.SelCo _ coercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion
  GHCTyCo.LRCo _ coercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion
  GHCTyCo.InstCo coercion instantiationCoercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion
      `appendCoreDirectDependencies` collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys instantiationCoercion
  GHCTyCo.KindCo coercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion
  GHCTyCo.SubCo coercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion
  GHCTyCo.AxiomRuleCo _ coercions ->
    concatCoreDirectDependencies
      (map (collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys) coercions)
  GHCTyCo.HoleCo _ ->
    emptyCoreDirectDependencies

collectDirectCoreDependenciesInMCoercion ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  GHCTyCo.MCoercion ->
  CoreDirectDependencies
collectDirectCoreDependenciesInMCoercion interestingDependencyKeys topLevelBindingKeys = \case
  GHCTyCo.MRefl ->
    emptyCoreDirectDependencies
  GHCTyCo.MCo coercion ->
    collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys coercion

coreDirectDependenciesForName ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  GHC.Name ->
  CoreDirectDependencies
coreDirectDependenciesForName interestingDependencyKeys topLevelBindingKeys name =
  let nameKey = nameUniqueKey name
   in CoreDirectDependencies
        { coreDirectSemanticNames =
            [ name
            | IntSet.member nameKey interestingDependencyKeys
            ],
          coreDirectTopLevelBinderKeys =
            [ nameKey
            | IntSet.member nameKey topLevelBindingKeys
            ]
        }

collectDirectCoreDependenciesInBind ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  GHC.CoreBind ->
  CoreDirectDependencies
collectDirectCoreDependenciesInBind interestingDependencyKeys topLevelBindingKeys = \case
  GHC.NonRec _ rhs ->
    collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys rhs
  GHC.Rec bindings ->
    concatCoreDirectDependencies
      [ collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys rhs
      | (_, rhs) <- bindings
      ]

collectDirectCoreDependenciesInAlt ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  GHC.CoreAlt ->
  CoreDirectDependencies
collectDirectCoreDependenciesInAlt interestingDependencyKeys topLevelBindingKeys (GHC.Alt alternativeConstructor _ expression) =
  coreDirectDependenciesForAltCon interestingDependencyKeys topLevelBindingKeys alternativeConstructor
    `appendCoreDirectDependencies` collectDirectCoreDependenciesInExpr interestingDependencyKeys topLevelBindingKeys expression

coreDirectDependenciesForAltCon ::
  IntSet.IntSet ->
  IntSet.IntSet ->
  GHC.AltCon ->
  CoreDirectDependencies
coreDirectDependenciesForAltCon interestingDependencyKeys topLevelBindingKeys = \case
  GHC.DataAlt dataCon ->
    coreDirectDependenciesForName interestingDependencyKeys topLevelBindingKeys (GHC.dataConName dataCon)
  GHC.LitAlt _ ->
    emptyCoreDirectDependencies
  GHC.DEFAULT ->
    emptyCoreDirectDependencies

dotFieldLabelRdrNamePs :: GHC.DotFieldOcc GHC.GhcPs -> GHC.RdrName
dotFieldLabelRdrNamePs dotFieldOccurrence =
  fieldLabelStringToRdrName (GHC.unLoc dotFieldOccurrence.dfoLabel)

dotFieldLabelRdrNameRn :: GHC.DotFieldOcc GHC.GhcRn -> GHC.RdrName
dotFieldLabelRdrNameRn dotFieldOccurrence =
  fieldLabelStringToRdrName (GHC.unLoc dotFieldOccurrence.dfoLabel)

fieldLabelStringToRdrName :: GHC.FieldLabelString -> GHC.RdrName
fieldLabelStringToRdrName fieldLabelString =
  GHC.mkRdrUnqual $
    GHC.mkVarOcc $
      GHC.showSDocUnsafe (GHC.ppr fieldLabelString)

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
  [MinimalTypedOccurrence] ->
  [DefinitionOccurrenceFact]
collectDefinitionOccurrenceFacts definingModule spans memberIndex typedOccurrences =
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
      guardReference definingModule spans occurrenceName $
        DefinitionOccurrenceFact
          { occurrenceFactName = occurrenceName,
            occurrenceFactSpan = occurrence.typedOccurrenceSpan,
            occurrenceFactOwners =
              chooseOccurrenceOwners
                memberIndex
                occurrence.typedOccurrenceParent
                occurrence.typedOccurrenceSpan,
            occurrenceFactParent = occurrence.typedOccurrenceParent
          }

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

dedupeOccurrences :: [DefinitionOccurrenceFact] -> [DefinitionOccurrenceFact]
dedupeOccurrences =
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.occurrenceFactName == right.occurrenceFactName
        && left.occurrenceFactSpan == right.occurrenceFactSpan
        && left.occurrenceFactOwners == right.occurrenceFactOwners
        && left.occurrenceFactParent == right.occurrenceFactParent

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

dedupeSemanticNamesByUnique :: [GHC.Name] -> [GHC.Name]
dedupeSemanticNamesByUnique =
  go IntSet.empty
  where
    go _ [] =
      []
    go seenKeys (name : names)
      | IntSet.member nameKey seenKeys =
          go seenKeys names
      | otherwise =
          name : go (IntSet.insert nameKey seenKeys) names
      where
        nameKey =
          nameUniqueKey name

nameUniqueKey :: GHC.Name -> Int
nameUniqueKey =
  fromIntegral . GHCUnique.getKey . GHC.getUnique

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
