module Lore.Bench.Memory
  ( MemoryResult (..),
    measureNF,
    measureIO,
    printMemoryResult,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Word (Word64)
import GHC.Stats
import System.Mem (performMajorGC)
import Text.Printf (printf)

data MemoryResult = MemoryResult
  { memoryCaseName :: String,
    allocatedBytes :: Word64,
    liveBytesBefore :: Word64,
    liveBytesAfter :: Word64,
    memInUseBytesBefore :: Word64,
    memInUseBytesAfter :: Word64
  }
  deriving stock (Eq, Show)

measureNF :: (NFData a) => String -> a -> IO MemoryResult
measureNF name value =
  measureIO name (pure value)

measureIO :: (NFData a) => String -> IO a -> IO MemoryResult
measureIO name action = do
  ensureStatsEnabled
  performMajorGC
  before <- getRTSStats
  value <- action >>= evaluate . force
  performMajorGC
  after <- getRTSStats
  _ <- evaluate value
  pure
    MemoryResult
      { memoryCaseName = name,
        allocatedBytes = deltaWord64 after.allocated_bytes before.allocated_bytes,
        liveBytesBefore = before.gc.gcdetails_live_bytes,
        liveBytesAfter = after.gc.gcdetails_live_bytes,
        memInUseBytesBefore = before.gc.gcdetails_mem_in_use_bytes,
        memInUseBytesAfter = after.gc.gcdetails_mem_in_use_bytes
      }

printMemoryResult :: MemoryResult -> IO ()
printMemoryResult result = do
  putStrLn result.memoryCaseName
  putStrLn $ "  allocated:          " <> prettyBytes result.allocatedBytes
  putStrLn $ "  live before GC:     " <> prettyBytes result.liveBytesBefore
  putStrLn $ "  live after GC:      " <> prettyBytes result.liveBytesAfter
  putStrLn $ "  mem in use before:  " <> prettyBytes result.memInUseBytesBefore
  putStrLn $ "  mem in use after:   " <> prettyBytes result.memInUseBytesAfter
  putStrLn $
    "MEMORY_RESULT\t"
      <> result.memoryCaseName
      <> "\t"
      <> show result.allocatedBytes
      <> "\t"
      <> show result.liveBytesBefore
      <> "\t"
      <> show result.liveBytesAfter
      <> "\t"
      <> show result.memInUseBytesBefore
      <> "\t"
      <> show result.memInUseBytesAfter

ensureStatsEnabled :: IO ()
ensureStatsEnabled = do
  enabled <- getRTSStatsEnabled
  if enabled
    then pure ()
    else
      fail
        "RTS stats are disabled. Run with +RTS -T or compile with -with-rtsopts=-T."

deltaWord64 :: Word64 -> Word64 -> Word64
deltaWord64 after before
  | after >= before = after - before
  | otherwise = 0

prettyBytes :: Word64 -> String
prettyBytes bytes =
  printf "%.2f MB (%d B)" (fromIntegral bytes / 1024 / 1024 :: Double) bytes
