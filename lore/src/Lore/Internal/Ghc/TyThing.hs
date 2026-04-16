{-# LANGUAGE LambdaCase #-}

module Lore.Internal.Ghc.TyThing
  ( isMentionedInTypeOfThing,
    isMentionedByOccName,
  )
where

import Control.Monad.State.Strict (State, evalState, gets, modify')
import qualified Data.Set as Set
import GHC.Core.Class
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.ConLike
import GHC.Core.DataCon (DataCon, EqSpec, dataConFullSig, dataConName, eqSpecPreds)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Types.Name (Name)
import GHC.Types.TyThing
import GHC.Types.Var
import Lore.Internal.Lookup.Name (NormalizedName (..), NormalizedOccName, normalizeName)

type VisitedNames = Set.Set Name

isMentionedInTypeOfThing :: (Name -> Bool) -> TyThing -> Bool
isMentionedInTypeOfThing predicate tyThing =
  evalState (mentionsInTyThing predicate tyThing) Set.empty

isMentionedByOccName :: NormalizedOccName -> TyThing -> Bool
isMentionedByOccName targetOccName =
  isMentionedInTypeOfThing (\name -> (normalizeName name).occName == targetOccName)

mentionsInTyThing :: (Name -> Bool) -> TyThing -> State VisitedNames Bool
mentionsInTyThing predicate = \case
  AnId identifier ->
    mentionsInVarType predicate identifier
  ATyCon tyCon ->
    mentionsInTyCon predicate tyCon
  AConLike conLike ->
    mentionsInConLike predicate conLike
  ACoAxiom axiom ->
    mentionsInCoAxiom predicate axiom

mentionsInTyCon :: (Name -> Bool) -> TyCon -> State VisitedNames Bool
mentionsInTyCon predicate tyCon =
  withFreshName predicate (tyConName tyCon) do
    anyM
      [ mentionsInType predicate (tyConKind tyCon),
        anyM (map (mentionsInTyConBinder predicate) (tyConBinders tyCon)),
        mentionsInClassPart predicate tyCon,
        mentionsInAlgPart predicate tyCon,
        mentionsInFamilyPart predicate tyCon
      ]

mentionsInClassPart :: (Name -> Bool) -> TyCon -> State VisitedNames Bool
mentionsInClassPart predicate tyCon =
  case tyConClass_maybe tyCon of
    Nothing -> pure False
    Just cls ->
      anyM
        [ anyM (map (mentionsInType predicate) (classSCTheta cls)),
          anyM (map (mentionsInVarType predicate) (classMethods cls)),
          anyM (map (mentionsInTyCon predicate) (classATs cls))
        ]

mentionsInAlgPart :: (Name -> Bool) -> TyCon -> State VisitedNames Bool
mentionsInAlgPart predicate tyCon =
  case tyConDataCons_maybe tyCon of
    Nothing -> pure False
    Just dataCons ->
      anyM (map (mentionsInDataCon predicate) dataCons)

mentionsInFamilyPart :: (Name -> Bool) -> TyCon -> State VisitedNames Bool
mentionsInFamilyPart predicate tyCon =
  case famTyConFlav_maybe tyCon of
    Nothing -> pure False
    Just flavor ->
      mentionsInFamTyConFlav predicate flavor

mentionsInFamTyConFlav :: (Name -> Bool) -> FamTyConFlav -> State VisitedNames Bool
mentionsInFamTyConFlav predicate = \case
  DataFamilyTyCon _ ->
    pure False
  OpenSynFamilyTyCon ->
    pure False
  ClosedSynFamilyTyCon maybeAxiom ->
    maybe (pure False) (mentionsInCoAxiom predicate) maybeAxiom
  AbstractClosedSynFamilyTyCon ->
    pure False
  BuiltInSynFamTyCon _ ->
    pure False

mentionsInCoAxiom :: (Name -> Bool) -> CoAxiom Branched -> State VisitedNames Bool
mentionsInCoAxiom predicate axiom =
  withFreshName predicate (coAxiomName axiom) do
    anyM (map (mentionsInCoAxBranch predicate) (fromBranches (coAxiomBranches axiom)))

mentionsInCoAxBranch :: (Name -> Bool) -> CoAxBranch -> State VisitedNames Bool
mentionsInCoAxBranch predicate branch =
  anyM
    [ anyM (map (mentionsInTyCoVar predicate) (cab_tvs branch)),
      anyM (map (mentionsInTyCoVar predicate) (cab_cvs branch)),
      anyM (map (mentionsInType predicate) (cab_lhs branch)),
      mentionsInType predicate (cab_rhs branch)
    ]

mentionsInConLike :: (Name -> Bool) -> ConLike -> State VisitedNames Bool
mentionsInConLike predicate conLike =
  withFreshName predicate (conLikeName conLike) do
    let (universalVars, existentialVars, eqSpecs, providedTheta, requiredTheta, argumentTypes, resultType) =
          conLikeFullSig conLike
    anyM
      [ anyM (map (mentionsInTyCoVar predicate) universalVars),
        anyM (map (mentionsInTyCoVar predicate) existentialVars),
        anyM (map (mentionsInEqSpec predicate) eqSpecs),
        anyM (map (mentionsInType predicate) providedTheta),
        anyM (map (mentionsInType predicate) requiredTheta),
        anyM (map (mentionsInScaledType predicate) argumentTypes),
        mentionsInType predicate resultType
      ]

mentionsInDataCon :: (Name -> Bool) -> DataCon -> State VisitedNames Bool
mentionsInDataCon predicate dataCon =
  withFreshName predicate (dataConName dataCon) do
    let (universalVars, existentialVars, eqSpecs, theta, argumentTypes, resultType) =
          dataConFullSig dataCon
    anyM
      [ anyM (map (mentionsInTyCoVar predicate) universalVars),
        anyM (map (mentionsInTyCoVar predicate) existentialVars),
        anyM (map (mentionsInEqSpec predicate) eqSpecs),
        anyM (map (mentionsInType predicate) theta),
        anyM (map (mentionsInScaledType predicate) argumentTypes),
        mentionsInType predicate resultType
      ]

mentionsInEqSpec :: (Name -> Bool) -> EqSpec -> State VisitedNames Bool
mentionsInEqSpec predicate eqSpec =
  anyM (map (mentionsInType predicate) (eqSpecPreds [eqSpec]))

mentionsInScaledType :: (Name -> Bool) -> Scaled Type -> State VisitedNames Bool
mentionsInScaledType predicate =
  mentionsInType predicate . scaledThing

mentionsInVarType :: (Name -> Bool) -> Var -> State VisitedNames Bool
mentionsInVarType =
  mentionsInTyCoVar

mentionsInTyConBinder :: (Name -> Bool) -> TyConBinder -> State VisitedNames Bool
mentionsInTyConBinder predicate binder =
  mentionsInTyCoVar predicate (binderVar binder)

mentionsInTyCoVar :: (Name -> Bool) -> TyCoVar -> State VisitedNames Bool
mentionsInTyCoVar predicate variable =
  withFreshName predicate (varName variable) do
    mentionsInType predicate (varType variable)

mentionsInType :: (Name -> Bool) -> Type -> State VisitedNames Bool
mentionsInType predicate = go
  where
    go ty =
      case splitForAllTyCoVars ty of
        ([], _) ->
          go' ty
        (typeVariables, rhoType) ->
          anyM
            [ anyM (map (mentionsInTyCoVar predicate) typeVariables),
              go' rhoType
            ]

    go' = \case
      TyVarTy typeVariable ->
        mentionsInTyCoVar predicate typeVariable
      AppTy functionType argumentType ->
        anyM
          [ go functionType,
            go argumentType
          ]
      TyConApp tyCon argumentTypes ->
        anyM
          [ pure (predicate (tyConName tyCon)),
            anyM (map go argumentTypes)
          ]
      FunTy _ multiplicity argumentType resultType ->
        anyM
          [ go multiplicity,
            go argumentType,
            go resultType
          ]
      ForAllTy binder bodyType ->
        anyM
          [ mentionsInTyCoVar predicate (binderVar binder),
            go bodyType
          ]
      LitTy _ ->
        pure False
      CastTy innerType coercion ->
        anyM
          [ go innerType,
            mentionsInCoercion predicate coercion
          ]
      CoercionTy coercion ->
        mentionsInCoercion predicate coercion

mentionsInCoercion :: (Name -> Bool) -> Coercion -> State VisitedNames Bool
mentionsInCoercion predicate = go
  where
    mentionsInMCoercion = \case
      MRefl -> pure False
      MCo coercion -> go coercion

    go = \case
      Refl ty ->
        mentionsInType predicate ty
      GRefl _ ty mco ->
        anyM
          [ mentionsInType predicate ty,
            mentionsInMCoercion mco
          ]
      TyConAppCo _ tyCon coercions ->
        anyM
          [ mentionsInTyCon predicate tyCon,
            anyM (map go coercions)
          ]
      AppCo coercionOne coercionTwo ->
        anyM
          [ go coercionOne,
            go coercionTwo
          ]
      ForAllCo typeVariable kindCoercion coercion ->
        anyM
          [ mentionsInTyCoVar predicate typeVariable,
            go kindCoercion,
            go coercion
          ]
      FunCo {fco_mult = multiplicityCoercion, fco_arg = argumentCoercion, fco_res = resultCoercion} ->
        anyM
          [ go multiplicityCoercion,
            go argumentCoercion,
            go resultCoercion
          ]
      CoVarCo coercionVariable ->
        mentionsInTyCoVar predicate coercionVariable
      AxiomInstCo {} ->
        pure False
      UnivCo _ _ typeOne typeTwo ->
        anyM
          [ mentionsInType predicate typeOne,
            mentionsInType predicate typeTwo
          ]
      SymCo coercion ->
        go coercion
      TransCo coercionOne coercionTwo ->
        anyM
          [ go coercionOne,
            go coercionTwo
          ]
      SelCo _ coercion ->
        go coercion
      LRCo _ coercion ->
        go coercion
      InstCo coercion instantiationCoercion ->
        anyM
          [ go coercion,
            go instantiationCoercion
          ]
      KindCo coercion ->
        go coercion
      SubCo coercion ->
        go coercion
      AxiomRuleCo {} ->
        pure False
      HoleCo {} ->
        pure False

withFreshName :: (Name -> Bool) -> Name -> State VisitedNames Bool -> State VisitedNames Bool
withFreshName predicate name continue =
  if predicate name
    then pure True
    else do
      alreadyVisited <- gets (Set.member name)
      if alreadyVisited
        then pure False
        else do
          modify' (Set.insert name)
          continue

anyM :: [State state Bool] -> State state Bool
anyM = \case
  [] ->
    pure False
  action : rest -> do
    matched <- action
    if matched
      then pure True
      else anyM rest
