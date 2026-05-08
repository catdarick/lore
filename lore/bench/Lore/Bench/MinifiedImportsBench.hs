{-# LANGUAGE RecordWildCards #-}

module Lore.Bench.MinifiedImportsBench
  ( benchmarks,
  )
where

import Criterion.Main
import Lore.Bench.Fixtures
import Lore.Internal.Definition.Analysis (buildMinifiedImports, normalizeImportItems)
import qualified Lore.Internal.Definition.Types as Def

benchmarks :: Benchmark
benchmarks =
  bgroup
    "minified-imports"
    [ bgroup
        "small"
        [ bench "buildMinifiedImports" $ nf runBuildMinifiedImports smallMinifiedImportsFixture,
          bench "normalizeImportItems" $ nf runNormalizeImportItems smallMinifiedImportsFixture
        ],
      bench "ambiguous/buildMinifiedImports" $ nf runBuildMinifiedImports ambiguousMinifiedImportsFixture,
      bench "large/buildMinifiedImports" $ nf runBuildMinifiedImports largeMinifiedImportsFixture
    ]

runBuildMinifiedImports :: MinifiedImportsFixture -> [Def.RequiredImport]
runBuildMinifiedImports MinifiedImportsFixture {..} =
  buildMinifiedImports minifiedImportCandidates minifiedOccurrences

runNormalizeImportItems :: MinifiedImportsFixture -> [Def.RequiredImportItem]
runNormalizeImportItems MinifiedImportsFixture {..} =
  normalizeImportItems (concatMap occurrenceItems minifiedOccurrences)
  where
    occurrenceItems occurrence =
      case Def.occurrenceFactParent occurrence of
        Just parentName
          | parentName /= Def.occurrenceFactName occurrence ->
              [Def.ImportParent parentName [Def.occurrenceFactName occurrence]]
        _ ->
          [Def.ImportName (Def.occurrenceFactName occurrence)]
