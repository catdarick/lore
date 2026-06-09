module ValueTypeHeadSpec
  ( spec,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC.Builtin.Types as GHC.Builtin
import qualified GHC.Builtin.Types.Prim as GHC.Prim
import qualified GHC.Core.TyCo.Rep as GHC.TyCo
import qualified GHC.Core.Type as GHC.Type
import qualified GHC.CoreToIface as GHC.Iface
import Lore.Internal.Ghc.ValueTypeHead (ValueTypeHeadNames (..), valueTypeHeadNamesFromIfaceType, valueTypeHeadNamesFromType)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "value type head extraction" do
    it "extracts argument and result heads from a simple function type" do
      factsFor (fun GHC.Builtin.intTy GHC.Builtin.boolTy)
        `shouldBe` heads_ ["Int"] ["Bool"]

    it "extracts nested result type heads" do
      factsFor (fun GHC.Builtin.intTy (maybeOf GHC.Builtin.boolTy))
        `shouldBe` heads_ ["Int"] ["Maybe", "Bool"]

    it "treats non-function values as result types" do
      factsFor (maybeOf GHC.Builtin.intTy)
        `shouldBe` heads_ [] ["Maybe", "Int"]

    it "extracts heads from higher-order arguments" do
      factsFor (fun (fun GHC.Builtin.boolTy GHC.Builtin.intTy) GHC.Builtin.stringTy)
        `shouldBe` heads_ ["Bool", "Int"] ["String"]

    it "ignores type variables under forall binders" do
      factsFor (GHC.Type.mkSpecForAllTy GHC.Prim.alphaTyVar (fun (GHC.TyCo.mkTyVarTy GHC.Prim.alphaTyVar) GHC.Builtin.boolTy))
        `shouldBe` heads_ [] ["Bool"]

    it "has semantic parity for interface types converted from Type" do
      let type_ = fun GHC.Builtin.intTy (maybeOf GHC.Builtin.boolTy)

      valueTypeHeadNamesFromIfaceType (GHC.Iface.toIfaceType type_)
        `shouldBe` valueTypeHeadNamesFromType type_

factsFor :: GHC.Type.Type -> ValueTypeHeadNames
factsFor =
  valueTypeHeadNamesFromType

fun :: GHC.Type.Type -> GHC.Type.Type -> GHC.Type.Type
fun =
  GHC.TyCo.mkVisFunTyMany

maybeOf :: GHC.Type.Type -> GHC.Type.Type
maybeOf type_ =
  GHC.Type.mkTyConApp GHC.Builtin.maybeTyCon [type_]

heads_ :: [String] -> [String] -> ValueTypeHeadNames
heads_ argumentStrings resultStrings =
  ValueTypeHeadNames
    { argumentTypeHeadNames = Set.fromList arguments,
      resultTypeHeadNames = Set.fromList results
    }
  where
    arguments = map T.pack argumentStrings
    results = map T.pack resultStrings
