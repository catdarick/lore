{-# LANGUAGE RecordWildCards #-}

module Lore.Bench.ReferenceSearchBench
  ( benchmarks,
    smokeBenchmarks,
  )
where

import Criterion.Main
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Bench.Fixtures
import Lore.Definition (dedupeReferenceHits, lookupModulesForOccurrenceKeys, matchingReferenceMatches)
import qualified Lore.Internal.Definition.Types as Def

benchmarks :: Benchmark
benchmarks =
  bgroup
    "reference-search"
    [ bgroup
        "rare-occ"
        [ bench "matchingReferenceMatches" $ nf runMatchingReferenceMatches smallReferenceSearchFixture,
          bench "dedupeReferenceHits" $ nf runDedupeReferenceHits smallReferenceSearchFixture,
          bench "lookupModulesForOccurrenceKeys" $ nf runLookupModulesForOccurrenceKeys smallReferenceSearchFixture
        ],
      bgroup
        "common-occ"
        [ bench "matchingReferenceMatches" $ nf runMatchingReferenceMatches commonOccReferenceSearchFixture,
          bench "lookupModulesForOccurrenceKeys" $ nf runLookupModulesForOccurrenceKeys commonOccReferenceSearchFixture
        ]
    ]

smokeBenchmarks :: Benchmark
smokeBenchmarks =
  bgroup
    "reference-search-smoke"
    [ bench "matchingReferenceMatches" $ nf runMatchingReferenceMatches smallReferenceSearchFixture
    ]

runMatchingReferenceMatches :: ReferenceSearchFixture -> [Def.ReferenceMatch]
runMatchingReferenceMatches ReferenceSearchFixture {..} =
  matchingReferenceMatches
    (Set.fromList referenceFixtureTargetNames)
    referenceFixtureModuleIndex

runDedupeReferenceHits :: ReferenceSearchFixture -> [Def.ReferenceHit]
runDedupeReferenceHits ReferenceSearchFixture {..} =
  dedupeReferenceHits referenceFixtureDuplicateHits

runLookupModulesForOccurrenceKeys :: ReferenceSearchFixture -> [GHC.Module]
runLookupModulesForOccurrenceKeys ReferenceSearchFixture {..} =
  lookupModulesForOccurrenceKeys
    (Set.fromList (map Def.nameOccKey referenceFixtureTargetNames))
    referenceFixtureOccurrenceIndex
