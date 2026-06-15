{-# LANGUAGE CPP #-}

module Lore.Internal.Lookup.TypeQuery.Names
  ( TypeQueryOccurrence (..),
    TypeQueryQualification (..),
    collectTypeQueryOccurrences,
  )
where

import Data.List (foldl')
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as Plugins

data TypeQueryOccurrence = TypeQueryOccurrence
  { typeQueryOccurrenceText :: !Text,
    typeQueryOccurrenceRdrName :: !GHC.RdrName,
    typeQueryOccurrenceQualification :: !TypeQueryQualification
  }

data TypeQueryQualification
  = TypeQueryUnqualified
  | TypeQueryQualified !GHC.ModuleName

collectTypeQueryOccurrences ::
  GHC.LHsType GHC.GhcPs ->
  Either Text [TypeQueryOccurrence]
collectTypeQueryOccurrences =
  collectTypeQueryOccurrencesWithBound Set.empty

{- ORMOLU_DISABLE -}
collectTypeQueryOccurrencesWithBound ::
  Set.Set GHC.RdrName ->
  GHC.LHsType GHC.GhcPs ->
  Either Text [TypeQueryOccurrence]
collectTypeQueryOccurrencesWithBound boundNames (GHC.L _ typeNode) =
  case typeNode of
    GHC.HsTyVar _ _ locatedName ->
      let rdrName = GHC.unLoc locatedName
       in if rdrName `Set.member` boundNames
            then Right []
            else Right [mkOccurrence rdrName]
    GHC.HsAppTy _ funcType argType ->
      combineOccurrences
        [ collectTypeQueryOccurrencesWithBound boundNames funcType,
          collectTypeQueryOccurrencesWithBound boundNames argType
        ]
#if MIN_VERSION_ghc(9,8,0)
    GHC.HsAppKindTy _ funcType _ kindType ->
#else
    GHC.HsAppKindTy _ funcType kindType ->
#endif
      combineOccurrences
        [ collectTypeQueryOccurrencesWithBound boundNames funcType,
          collectTypeQueryOccurrencesWithBound boundNames kindType
        ]
    GHC.HsOpTy _ _ leftType opName rightType ->
      combineOccurrences
        [ collectTypeQueryOccurrencesWithBound boundNames leftType,
          Right [mkOccurrence (GHC.unLoc opName)],
          collectTypeQueryOccurrencesWithBound boundNames rightType
        ]
    GHC.HsParTy _ innerType ->
      collectTypeQueryOccurrencesWithBound boundNames innerType
    GHC.HsKindSig _ innerType kindType ->
      combineOccurrences
        [ collectTypeQueryOccurrencesWithBound boundNames innerType,
          collectTypeQueryOccurrencesWithBound boundNames kindType
        ]
    GHC.HsQualTy _ context bodyType ->
      combineOccurrences
        [ collectContextOccurrences boundNames context,
          collectTypeQueryOccurrencesWithBound boundNames bodyType
        ]
    GHC.HsForAllTy _ telescope bodyType -> do
      (extendedBoundNames, binderOccurrences) <- collectTelescopeOccurrences boundNames telescope
      bodyOccurrences <- collectTypeQueryOccurrencesWithBound extendedBoundNames bodyType
      pure (binderOccurrences <> bodyOccurrences)
    GHC.HsFunTy _ _ argType resultType ->
      combineOccurrences
        [ collectTypeQueryOccurrencesWithBound boundNames argType,
          collectTypeQueryOccurrencesWithBound boundNames resultType
        ]
    GHC.HsListTy _ innerType ->
      collectTypeQueryOccurrencesWithBound boundNames innerType
    GHC.HsTupleTy _ _ tupleTypes ->
      combineOccurrences (map (collectTypeQueryOccurrencesWithBound boundNames) tupleTypes)
    GHC.HsExplicitListTy _ _ explicitTypes ->
      combineOccurrences (map (collectTypeQueryOccurrencesWithBound boundNames) explicitTypes)
    GHC.HsExplicitTupleTy _ tupleTypes ->
      combineOccurrences (map (collectTypeQueryOccurrencesWithBound boundNames) tupleTypes)
    GHC.HsSumTy _ sumTypes ->
      combineOccurrences (map (collectTypeQueryOccurrencesWithBound boundNames) sumTypes)
    GHC.HsDocTy _ innerType _ ->
      collectTypeQueryOccurrencesWithBound boundNames innerType
    GHC.HsBangTy _ _ innerType ->
      collectTypeQueryOccurrencesWithBound boundNames innerType
    GHC.HsTyLit {} ->
      Right []
    GHC.HsWildCardTy {} ->
      Right []
    GHC.HsStarTy {} ->
      Right []
    GHC.HsIParamTy {} ->
      Left (unsupportedTypeQuerySyntax "implicit parameter types are not supported in type queries")
    GHC.HsSpliceTy {} ->
      Left (unsupportedTypeQuerySyntax "type splices are not supported in type queries")
    GHC.HsRecTy {} ->
      Left (unsupportedTypeQuerySyntax "record types are not supported in type queries")
    GHC.XHsType {} ->
      Left (unsupportedTypeQuerySyntax "extended type syntax is not supported in type queries")
{- ORMOLU_ENABLE -}

collectContextOccurrences ::
  Set.Set GHC.RdrName ->
  GHC.LHsContext GHC.GhcPs ->
  Either Text [TypeQueryOccurrence]
collectContextOccurrences boundNames contextTypes =
  combineOccurrences (map (collectTypeQueryOccurrencesWithBound boundNames) (GHC.unLoc contextTypes))

collectTelescopeOccurrences ::
  Set.Set GHC.RdrName ->
  GHC.HsForAllTelescope GHC.GhcPs ->
  Either Text (Set.Set GHC.RdrName, [TypeQueryOccurrence])
collectTelescopeOccurrences initialBoundNames telescope =
  case telescope of
    GHC.HsForAllVis _ binders ->
      foldTelescopeBinders initialBoundNames binders
    GHC.HsForAllInvis _ binders ->
      foldTelescopeBinders initialBoundNames binders

foldTelescopeBinders ::
  Set.Set GHC.RdrName ->
  [GHC.LHsTyVarBndr flag GHC.GhcPs] ->
  Either Text (Set.Set GHC.RdrName, [TypeQueryOccurrence])
foldTelescopeBinders initialBoundNames binders =
  foldl'
    collectOneBinder
    (Right (initialBoundNames, []))
    binders
  where
    collectOneBinder eiAccum locatedBinder = do
      (boundNames, occurrences) <- eiAccum
      (binderName, binderOccurrences) <- collectBinderOccurrences boundNames locatedBinder
      pure (Set.insert binderName boundNames, occurrences <> binderOccurrences)

collectBinderOccurrences ::
  Set.Set GHC.RdrName ->
  GHC.LHsTyVarBndr flag GHC.GhcPs ->
  Either Text (GHC.RdrName, [TypeQueryOccurrence])
collectBinderOccurrences boundNames (GHC.L _ binder) =
  case binder of
    GHC.UserTyVar _ _ binderName ->
      pure (GHC.unLoc binderName, [])
    GHC.KindedTyVar _ _ binderName kindType -> do
      kindOccurrences <- collectTypeQueryOccurrencesWithBound boundNames kindType
      pure (GHC.unLoc binderName, kindOccurrences)

mkOccurrence :: GHC.RdrName -> TypeQueryOccurrence
mkOccurrence rdrName =
  TypeQueryOccurrence
    { typeQueryOccurrenceText = renderRdrName rdrName,
      typeQueryOccurrenceRdrName = rdrName,
      typeQueryOccurrenceQualification =
        case Plugins.isQual_maybe rdrName of
          Just (moduleName, _) ->
            TypeQueryQualified moduleName
          Nothing ->
            TypeQueryUnqualified
    }

combineOccurrences ::
  [Either Text [TypeQueryOccurrence]] ->
  Either Text [TypeQueryOccurrence]
combineOccurrences =
  fmap concat . sequence

renderRdrName :: GHC.RdrName -> Text
renderRdrName =
  T.pack . Plugins.showSDocUnsafe . Plugins.ppr

unsupportedTypeQuerySyntax :: Text -> Text
unsupportedTypeQuerySyntax message =
  "Unsupported type query syntax: " <> message <> "."
