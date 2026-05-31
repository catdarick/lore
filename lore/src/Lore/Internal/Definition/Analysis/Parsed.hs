module Lore.Internal.Definition.Analysis.Parsed
  ( buildParsedModuleFacts,
    collectParsedOccurrenceNames,
    collectParsedDefinitionMembers,
  )
where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Data.Strict as Strict
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Analysis.Common
  ( collectLocatedRdrNames,
    collectTyped,
    dotFieldLabelRdrNamePs,
  )
import Lore.Internal.Definition.SourceTree (collectModuleSourceRegionCandidates)
import Lore.Internal.Definition.Types

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

dedupeParsedDefinitionMembers :: [ParsedDefinitionMember] -> [ParsedDefinitionMember]
dedupeParsedDefinitionMembers =
  List.nubBy sameMember
  where
    sameMember left right =
      left.parsedMemberOccKey == right.parsedMemberOccKey
        && left.parsedMemberSpan == right.parsedMemberSpan
