{-# LANGUAGE RecordWildCards #-}

module Lore.Bench.RenderingBench
  ( benchmarks,
  )
where

import Criterion.Main
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Bench.Fixtures
import Lore.Internal.Definition.SourceTree
  ( buildDefinitionSourceTree,
    chooseBestReferenceContext,
    flattenSourceRegions,
    nestSourceRegions,
  )
import qualified Lore.Internal.Definition.Types as Def

benchmarks :: Benchmark
benchmarks =
  bgroup
    "rendering"
    [ bgroup
        "small"
        [ bench "buildDefinitionSourceTree" $ nf runBuildDefinitionSourceTree smallSourceRegionFixture,
          bench "nestSourceRegions" $ nf runNestSourceRegions smallSourceRegionFixture,
          bench "chooseBestReferenceContext" $ nf runChooseBestReferenceContext smallSourceRegionFixture
        ],
      bgroup
        "large"
        [ bench "buildDefinitionSourceTree" $ nf runBuildDefinitionSourceTree largeSourceRegionFixture,
          bench "nestSourceRegions" $ nf runNestSourceRegions largeSourceRegionFixture,
          bench "flattenSourceRegions" $ nf runFlattenSourceRegions largeSourceRegionFixture
        ]
    ]

runBuildDefinitionSourceTree :: SourceRegionFixture -> Def.SourceRegion
runBuildDefinitionSourceTree SourceRegionFixture {..} =
  buildDefinitionSourceTree sourceRegionFixtureDeclaration sourceRegionFixtureCandidates

runNestSourceRegions :: SourceRegionFixture -> [Def.SourceRegion]
runNestSourceRegions SourceRegionFixture {..} =
  nestSourceRegions sourceRegionFixtureCandidates

runFlattenSourceRegions :: SourceRegionFixture -> [Def.SourceRegion]
runFlattenSourceRegions fixture =
  flattenSourceRegions (runBuildDefinitionSourceTree fixture)

runChooseBestReferenceContext :: SourceRegionFixture -> Maybe GHC.SrcSpan
runChooseBestReferenceContext fixture@SourceRegionFixture {..} =
  chooseBestReferenceContext (mkSourceTree fixture) sourceRegionFixtureReferenceSpan

mkSourceTree :: SourceRegionFixture -> Def.DefinitionSourceTree
mkSourceTree fixture =
  Def.DefinitionSourceTree
    { Def.sourceTreeDefinition = definitionSource,
      Def.sourceTreeRoot = runBuildDefinitionSourceTree fixture
    }
  where
    benchModule = mkBenchModule "Fixture.Bench.Rendering"
    name = mkBenchName benchModule "renderTarget"
    declaration = sourceRegionFixtureDeclaration fixture
    definitionSource =
      Def.DefinitionSource
        { Def.definitionSourceId = mkBenchDefinitionId benchModule (Def.declarationSpan declaration),
          Def.definitionSourceModule = benchModule,
          Def.definitionSourceNames = Set.singleton name,
          Def.definitionSourceSpans = declaration
        }
