module Main where

import Criterion.Main
import qualified Lore.Bench.DefinitionIndexBench as DefinitionIndexBench
import qualified Lore.Bench.E2EBench as E2EBench
import qualified Lore.Bench.MinifiedImportsBench as MinifiedImportsBench
import qualified Lore.Bench.ReferenceSearchBench as ReferenceSearchBench
import qualified Lore.Bench.RenderingBench as RenderingBench
import System.Environment (lookupEnv)

main :: IO ()
main = do
  mode <- lookupEnv "BENCH_MODE"
  defaultMain $
    case mode of
      Just "smoke" -> smokeBenchmarks
      Just "pure" -> pureBenchmarks
      Just "e2e" -> e2eBenchmarks
      _ -> fullBenchmarks

pureBenchmarks :: [Benchmark]
pureBenchmarks =
  [ DefinitionIndexBench.benchmarks,
    MinifiedImportsBench.benchmarks,
    ReferenceSearchBench.benchmarks,
    RenderingBench.benchmarks
  ]

e2eBenchmarks :: [Benchmark]
e2eBenchmarks =
  [E2EBench.benchmarks]

fullBenchmarks :: [Benchmark]
fullBenchmarks =
  pureBenchmarks <> e2eBenchmarks

smokeBenchmarks :: [Benchmark]
smokeBenchmarks =
  [ DefinitionIndexBench.smokeBenchmarks,
    ReferenceSearchBench.smokeBenchmarks,
    E2EBench.smokeBenchmarks
  ]
