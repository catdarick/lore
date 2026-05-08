module MemoryMain
  ( main,
  )
where

import Lore.Bench.MemoryCases (runMemoryBenchmarks)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  maybeMode <- lookupEnv "MEMORY_MODE"
  runMemoryBenchmarks maybeMode
