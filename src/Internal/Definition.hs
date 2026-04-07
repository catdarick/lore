module Internal.Definition where

import Control.Applicative ((<|>))
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified GHC
import qualified GHC.Plugins as GHC
import Internal.Lookup.ModSummaries (getModSummaries)
import Internal.Lookup.Types (ModSummaries (..))
import Monad

data DefinitionSpans = DefinitionSpans
  { declarationSpan :: GHC.SrcSpan,
    signatureSpan :: Maybe GHC.SrcSpan
  }
  deriving stock (Eq, Show)

resolveDefinitionSpans :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSpans)
resolveDefinitionSpans inputName = do
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
          pure $ findDefinitionSpansInParsedSource inputName parsedModule

--------------------------------------------------------------------------------
-- Parsed source lookup

findDefinitionSpansInParsedSource :: GHC.Name -> GHC.ParsedModule -> Maybe DefinitionSpans
findDefinitionSpansInParsedSource target parsedModule =
  findDefinitionSpansInModule target (GHC.pm_parsed_source parsedModule)

findDefinitionSpansInModule :: GHC.Name -> GHC.ParsedSource -> Maybe DefinitionSpans
findDefinitionSpansInModule target parsedSource = do
  parentDecl <- mParentDecl
  pure
    DefinitionSpans
      { declarationSpan = GHC.getLocA parentDecl,
        signatureSpan = GHC.getLocA <$> signatureDecl
      }
  where
    decls :: [GHC.LHsDecl GHC.GhcPs]
    decls =
      GHC.hsmodDecls $
        GHC.unLoc parsedSource

    targetOcc :: GHC.OccName
    targetOcc = GHC.nameOccName target

    mParentDecl :: Maybe (GHC.LHsDecl GHC.GhcPs)
    mParentDecl =
      findParentDeclaration target decls

    signatureDecl :: Maybe (GHC.LHsDecl GHC.GhcPs)
    signatureDecl =
      findSignatureDeclaration targetOcc decls

findParentDeclaration ::
  GHC.Name ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Maybe (GHC.LHsDecl GHC.GhcPs)
findParentDeclaration target decls =
  findByContainingNameSpan target decls
    <|> findByTopLevelBinderOcc (GHC.nameOccName target) decls

findSignatureDeclaration ::
  GHC.OccName ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Maybe (GHC.LHsDecl GHC.GhcPs)
findSignatureDeclaration targetOcc =
  find (isTopLevelSignatureFor targetOcc)

--------------------------------------------------------------------------------
-- Parent declaration matching

findByContainingNameSpan ::
  GHC.Name ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Maybe (GHC.LHsDecl GHC.GhcPs)
findByContainingNameSpan target decls =
  case GHC.srcSpanToRealSrcSpan (GHC.nameSrcSpan target) of
    Nothing ->
      Nothing
    Just _ ->
      find (\decl -> GHC.nameSrcSpan target `GHC.isSubspanOf` GHC.getLocA decl) decls

findByTopLevelBinderOcc ::
  GHC.OccName ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Maybe (GHC.LHsDecl GHC.GhcPs)
findByTopLevelBinderOcc targetOcc =
  find (declaresTopLevelOcc targetOcc)

declaresTopLevelOcc :: GHC.OccName -> GHC.LHsDecl GHC.GhcPs -> Bool
declaresTopLevelOcc targetOcc decl =
  case GHC.unLoc decl of
    GHC.ValD _ bind ->
      any (matchesRdrName targetOcc) $
        GHC.collectHsBindBinders GHC.CollNoDictBinders bind
    GHC.TyClD _ tyClDecl ->
      matchesLocatedRdrName targetOcc (tyClDeclBinder tyClDecl)
    GHC.ForD _ foreignDecl ->
      matchesLocatedRdrName targetOcc (binderOfForeignDecl foreignDecl)
    _ ->
      False

matchesLocatedRdrName ::
  GHC.OccName ->
  GHC.GenLocated l GHC.RdrName ->
  Bool
matchesLocatedRdrName targetOcc =
  matchesRdrName targetOcc . GHC.unLoc

matchesRdrName :: GHC.OccName -> GHC.RdrName -> Bool
matchesRdrName targetOcc rdrName =
  GHC.rdrNameOcc rdrName == targetOcc

tyClDeclBinder :: GHC.TyClDecl GHC.GhcPs -> GHC.LIdP GHC.GhcPs
tyClDeclBinder = \case
  GHC.FamDecl {GHC.tcdFam = GHC.FamilyDecl {GHC.fdLName}} -> fdLName
  GHC.SynDecl {GHC.tcdLName} -> tcdLName
  GHC.DataDecl {GHC.tcdLName} -> tcdLName
  GHC.ClassDecl {GHC.tcdLName} -> tcdLName

binderOfForeignDecl :: GHC.ForeignDecl GHC.GhcPs -> GHC.LIdP GHC.GhcPs
binderOfForeignDecl = \case
  GHC.ForeignImport {GHC.fd_name} -> fd_name
  GHC.ForeignExport {GHC.fd_name} -> fd_name

--------------------------------------------------------------------------------
-- Signature matching

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
