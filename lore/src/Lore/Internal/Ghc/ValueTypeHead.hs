{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Ghc.ValueTypeHead
  ( ValueTypeHeadNames (..),
    mergeValueTypeHeadNames,
    valueTypeHeadNamesFromType,
    valueTypeHeadNamesFromIfaceType,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Core.TyCo.Rep as GHC.TyCo
import qualified GHC.Core.Type as GHC.Type
import GHC.Generics (Generic)
import qualified GHC.Iface.Type as GHC.Iface
import qualified GHC.Types.Name as GHC.Name
import qualified GHC.Types.Name.Occurrence as GHC.Occ
import qualified GHC.Types.Var as GHC.Var

data ValueTypeHeadNames = ValueTypeHeadNames
  { argumentTypeHeadNames :: !(Set.Set Text),
    resultTypeHeadNames :: !(Set.Set Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

mergeValueTypeHeadNames :: ValueTypeHeadNames -> ValueTypeHeadNames -> ValueTypeHeadNames
mergeValueTypeHeadNames left right =
  ValueTypeHeadNames
    { argumentTypeHeadNames = left.argumentTypeHeadNames <> right.argumentTypeHeadNames,
      resultTypeHeadNames = left.resultTypeHeadNames <> right.resultTypeHeadNames
    }

valueTypeHeadNamesFromType :: GHC.Type -> ValueTypeHeadNames
valueTypeHeadNamesFromType type_ =
  ValueTypeHeadNames
    { argumentTypeHeadNames = foldMap collectTypeHeadNames argumentTypes,
      resultTypeHeadNames = collectTypeHeadNames resultType
    }
  where
    (argumentTypes, resultType) = splitValueFunctionType (stripForalls type_)

    stripForalls ty =
      snd (GHC.splitForAllTyCoVars ty)

    splitValueFunctionType = \case
      GHC.TyCo.FunTy {GHC.TyCo.ft_af = argFlag, GHC.TyCo.ft_arg = argType, GHC.TyCo.ft_res = resType} ->
        let (restArgs, finalResult) = splitValueFunctionType resType
            args =
              if isConstraintArgument argFlag || isConstraintType argType
                then restArgs
                else argType : restArgs
         in (args, finalResult)
      ty ->
        ([], ty)

    collectTypeHeadNames = \case
      GHC.TyCo.TyVarTy _ ->
        Set.empty
      GHC.TyCo.AppTy function argument ->
        collectTypeHeadNames function <> collectTypeHeadNames argument
      GHC.TyCo.TyConApp tyCon arguments ->
        maybe Set.empty Set.singleton (userFacingOccText (GHC.getName tyCon))
          <> foldMap collectTypeHeadNames arguments
      GHC.TyCo.ForAllTy _ body ->
        collectTypeHeadNames body
      GHC.TyCo.FunTy {GHC.TyCo.ft_arg = argType, GHC.TyCo.ft_res = resType} ->
        collectTypeHeadNames argType <> collectTypeHeadNames resType
      GHC.TyCo.LitTy _ ->
        Set.empty
      GHC.TyCo.CastTy ty _ ->
        collectTypeHeadNames ty
      GHC.TyCo.CoercionTy _ ->
        Set.empty

valueTypeHeadNamesFromIfaceType :: GHC.Iface.IfaceType -> ValueTypeHeadNames
valueTypeHeadNamesFromIfaceType type_ =
  ValueTypeHeadNames
    { argumentTypeHeadNames = foldMap collectIfaceTypeHeadNames argumentTypes,
      resultTypeHeadNames = collectIfaceTypeHeadNames resultType
    }
  where
    (argumentTypes, resultType) = splitValueFunctionType (stripForalls type_)

    stripForalls = \case
      GHC.Iface.IfaceForAllTy _ body -> stripForalls body
      ty -> ty

    splitValueFunctionType = \case
      GHC.Iface.IfaceFunTy argFlag _ argType resType ->
        let (restArgs, finalResult) = splitValueFunctionType resType
            args =
              if isConstraintArgument argFlag
                then restArgs
                else argType : restArgs
         in (args, finalResult)
      ty ->
        ([], ty)

    collectIfaceTypeHeadNames = \case
      GHC.Iface.IfaceFreeTyVar _ ->
        Set.empty
      GHC.Iface.IfaceTyVar _ ->
        Set.empty
      GHC.Iface.IfaceLitTy _ ->
        Set.empty
      GHC.Iface.IfaceAppTy function args ->
        collectIfaceTypeHeadNames function <> collectIfaceAppArgsHeadNames args
      GHC.Iface.IfaceFunTy _ _ argType resType ->
        collectIfaceTypeHeadNames argType <> collectIfaceTypeHeadNames resType
      GHC.Iface.IfaceForAllTy _ body ->
        collectIfaceTypeHeadNames body
      GHC.Iface.IfaceTyConApp tyCon args ->
        maybe Set.empty Set.singleton (userFacingOccText (GHC.Iface.ifaceTyConName tyCon))
          <> collectIfaceAppArgsHeadNames args
      GHC.Iface.IfaceCastTy ty _ ->
        collectIfaceTypeHeadNames ty
      GHC.Iface.IfaceCoercionTy _ ->
        Set.empty
      GHC.Iface.IfaceTupleTy _ _ args ->
        collectIfaceAppArgsHeadNames args

    collectIfaceAppArgsHeadNames = \case
      GHC.Iface.IA_Nil ->
        Set.empty
      GHC.Iface.IA_Arg argType _ rest ->
        collectIfaceTypeHeadNames argType <> collectIfaceAppArgsHeadNames rest

isConstraintArgument :: GHC.Var.FunTyFlag -> Bool
isConstraintArgument flag =
  flag == GHC.Var.FTF_C_T || flag == GHC.Var.FTF_C_C

isConstraintType :: GHC.Type -> Bool
#if MIN_VERSION_ghc(9,14,0)
isConstraintType = GHC.Type.isConstraintLikeKind . GHC.Type.typeKind
#else
isConstraintType = GHC.Type.isPredTy
#endif

userFacingOccText :: GHC.Name -> Maybe Text
userFacingOccText name
  | GHC.Name.isSystemName name = Nothing
  | otherwise =
      let text = T.pack (GHC.Occ.occNameString (GHC.Name.nameOccName name))
       in if T.null text || T.any generatedNameChar text
            then Nothing
            else Just text
  where
    generatedNameChar char =
      char == '#' || char == '$'
