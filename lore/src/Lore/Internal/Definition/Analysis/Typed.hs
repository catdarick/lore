module Lore.Internal.Definition.Analysis.Typed
  ( buildMinimalTypedModuleFacts,
    collectMinimalTypedOccurrences,
  )
where

import Data.Containers.ListUtils (nubOrd)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC
import qualified GHC.Core.FamInstEnv as GHC.FamInst
import qualified GHC.Core.InstEnv as GHC.InstEnv
import qualified GHC.Core.TyCo.FVs as GHC.TyCoFVs
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import qualified GHC.Types.Avail as GHC
import qualified GHC.Types.FieldLabel as GHC.FieldLabel
import qualified GHC.Types.Unique.Set as GHC.UniqueSet
import Lore.Internal.Definition.Analysis.Common
  ( collectTyped,
    dotFieldLabelRdrNameRn,
    locatedSpan,
  )
import Lore.Internal.Definition.Types
import Lore.Internal.Ghc.AvailInfo (availInfoGreNames, availInfoNamesWithFields, fieldLabelAliasText, greNameFieldAliasText)

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

dedupeMinimalTypedOccurrences :: [MinimalTypedOccurrence] -> [MinimalTypedOccurrence]
dedupeMinimalTypedOccurrences =
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.typedOccurrenceName == right.typedOccurrenceName
        && left.typedOccurrenceSpan == right.typedOccurrenceSpan
        && left.typedOccurrenceParent == right.typedOccurrenceParent
