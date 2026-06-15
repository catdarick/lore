{-# LANGUAGE CPP #-}

module Lore.Internal.Definition.Analysis.Core
  ( buildCoreDependenciesByBinder,
  )
where

import qualified Data.Foldable as Foldable
import qualified Data.Graph as Graph
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Core.Coercion.Axiom as GHCAxiom
import qualified GHC.Core.TyCo.FVs as GHC.TyCoFVs
import qualified GHC.Core.TyCo.Rep as GHCTyCo
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Unique.Set as GHC.UniqueSet
import Lore.Internal.Definition.Analysis.Common (nameUniqueKey)

buildCoreDependenciesByBinder ::
  Set.Set GHC.Name ->
  Set.Set GHC.Name ->
  [GHC.CoreBind] ->
  (Map.Map GHC.Name [GHC.Name], Map.Map GHC.Name [GHC.Name])
buildCoreDependenciesByBinder interestingBinders interestingDependencyNames coreBinds =
  (evidenceDependenciesByBinder, semanticDependenciesByBinder)
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

    evidenceDependenciesByBinder =
      Map.fromList
        [ (binderName, evidenceDependencies)
        | (binderKey, (binderName, _)) <- IntMap.toList topLevelBindingsByKey,
          IntSet.member binderKey interestingBinderKeys,
          Just directDependencies <- [IntMap.lookup binderKey directDependenciesByKey],
          let evidenceDependencies =
                dedupeSemanticNamesByUnique directDependencies.coreDirectEvidenceNames,
          not (null evidenceDependencies)
        ]

    semanticDependenciesByKey =
      buildTransitiveCoreSemanticDependencies directDependenciesByKey

    semanticDependenciesByBinder =
      Map.fromList
        [ (binderName, semanticDependencies)
        | (binderKey, (binderName, _)) <- IntMap.toList topLevelBindingsByKey,
          IntSet.member binderKey interestingBinderKeys,
          Just semanticDependencies <- [IntMap.lookup binderKey semanticDependenciesByKey],
          not (null semanticDependencies)
        ]

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
      Foldable.foldl'
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
    coreDirectTopLevelBinderKeys :: [Int],
    coreDirectEvidenceNames :: [GHC.Name]
  }

emptyCoreDirectDependencies :: CoreDirectDependencies
emptyCoreDirectDependencies =
  CoreDirectDependencies
    { coreDirectSemanticNames = [],
      coreDirectTopLevelBinderKeys = [],
      coreDirectEvidenceNames = []
    }

appendCoreDirectDependencies :: CoreDirectDependencies -> CoreDirectDependencies -> CoreDirectDependencies
appendCoreDirectDependencies left right =
  CoreDirectDependencies
    { coreDirectSemanticNames =
        left.coreDirectSemanticNames <> right.coreDirectSemanticNames,
      coreDirectTopLevelBinderKeys =
        left.coreDirectTopLevelBinderKeys <> right.coreDirectTopLevelBinderKeys,
      coreDirectEvidenceNames =
        left.coreDirectEvidenceNames <> right.coreDirectEvidenceNames
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
                  variableName : variableDependencies.coreDirectSemanticNames,
                coreDirectEvidenceNames =
                  variableName : variableDependencies.coreDirectEvidenceNames
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

{- ORMOLU_DISABLE -}
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
#if MIN_VERSION_ghc(9,10,0)
  GHCTyCo.ForAllCo {GHCTyCo.fco_kind = kindCoercion, GHCTyCo.fco_body = coercion} ->
#else
  GHCTyCo.ForAllCo _ kindCoercion coercion ->
#endif
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
#if MIN_VERSION_ghc(9,12,0)
  GHCTyCo.AxiomCo axiomRule coercions ->
    maybe
      emptyCoreDirectDependencies
      (coreDirectDependenciesForName interestingDependencyKeys topLevelBindingKeys)
      (coAxiomRuleName axiomRule)
      `appendCoreDirectDependencies` concatCoreDirectDependencies
        (map (collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys) coercions)
  GHCTyCo.UnivCo {GHCTyCo.uco_deps = dependencies} ->
    concatCoreDirectDependencies
      (map (collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys) dependencies)
#else
  GHCTyCo.AxiomInstCo axiom _ coercions ->
    coreDirectDependenciesForName interestingDependencyKeys topLevelBindingKeys (GHCAxiom.coAxiomName axiom)
      `appendCoreDirectDependencies` concatCoreDirectDependencies
        (map (collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys) coercions)
  GHCTyCo.UnivCo _ _ _ _ ->
    emptyCoreDirectDependencies
#endif
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
#if !MIN_VERSION_ghc(9,12,0)
  GHCTyCo.AxiomRuleCo _ coercions ->
    concatCoreDirectDependencies
      (map (collectDirectCoreDependenciesInCoercion interestingDependencyKeys topLevelBindingKeys) coercions)
#endif
  GHCTyCo.HoleCo _ ->
    emptyCoreDirectDependencies
{- ORMOLU_ENABLE -}

#if MIN_VERSION_ghc(9,12,0)
coAxiomRuleName :: GHCAxiom.CoAxiomRule -> Maybe GHC.Name
coAxiomRuleName = \case
  GHCAxiom.BranchedAxiom axiom _ -> Just (GHCAxiom.coAxiomName axiom)
  GHCAxiom.UnbranchedAxiom axiom -> Just (GHCAxiom.coAxiomName axiom)
  GHCAxiom.BuiltInFamRew _ -> Nothing
  GHCAxiom.BuiltInFamInj _ -> Nothing
#endif

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
            ],
          coreDirectEvidenceNames = []
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
