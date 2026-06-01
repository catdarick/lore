module Lore.Tools.DebugCacheMemory
  ( debugCacheMemory,
    renderDebugCacheMemory,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Lore as Core
import Lore.Tools.Render.Doc
  ( LoreDoc,
    heading2,
    heading3,
    numberedListFrom,
    paragraph,
  )
import Numeric (showFFloat)

debugCacheMemory :: (Core.MonadLore m) => m Core.CacheMemoryDebugResult
debugCacheMemory =
  Core.debugSessionCachesMemory

renderDebugCacheMemory :: Core.CacheMemoryDebugResult -> LoreDoc
renderDebugCacheMemory result
  | not result.cacheMemoryDebugRtsStatsEnabled =
      heading2 "Cache Memory Debug"
        <> paragraph "RTS stats are disabled. Re-run with +RTS -T -RTS."
  | otherwise =
      heading2 "Cache Memory Debug"
        <> paragraph
          ( "Caches checked: "
              <> T.pack (show (length sortedSamples))
              <> ", major GC rounds per cache: "
              <> T.pack (show result.cacheMemoryDebugGcRounds)
              <> ", delay per round: "
              <> renderSeconds result.cacheMemoryDebugGcDelayMicros
          )
        <> maybe mempty renderPreBaseline result.cacheMemoryDebugPreBaseline
        <> maybe mempty (renderBaseline result.cacheMemoryDebugPreBaseline) result.cacheMemoryDebugBaseline
        <> heading3 "Most Memory-Releasing Caches First (mem_in_use delta)"
        <> numberedListFrom 1 (map renderSample sortedSamples)
  where
    sortedSamples =
      sortOn (Down . cacheMemInUseBytesFreed) result.cacheMemoryDebugSamples

renderPreBaseline :: Core.CacheMemorySnapshot -> LoreDoc
renderPreBaseline snapshot =
  paragraph $
    "Pre-baseline RTS: live "
      <> renderBytes snapshot.cacheMemorySnapshotLiveBytes
      <> ", mem_in_use "
      <> renderBytes snapshot.cacheMemorySnapshotMemInUseBytes
      <> ", major_gcs "
      <> T.pack (show snapshot.cacheMemorySnapshotMajorGcs)

renderBaseline :: Maybe Core.CacheMemorySnapshot -> Core.CacheMemorySnapshot -> LoreDoc
renderBaseline maybePreBaseline baselineSnapshot =
  paragraph $
    "Baseline RTS after pre-GC rounds: live "
      <> renderBytes baselineSnapshot.cacheMemorySnapshotLiveBytes
      <> " ("
      <> maybe "+0.00 MiB" (renderSnapshotDelta (.cacheMemorySnapshotLiveBytes) baselineSnapshot) maybePreBaseline
      <> "), mem_in_use "
      <> renderBytes baselineSnapshot.cacheMemorySnapshotMemInUseBytes
      <> " ("
      <> maybe "+0.00 MiB" (renderSnapshotDelta (.cacheMemorySnapshotMemInUseBytes) baselineSnapshot) maybePreBaseline
      <> "), major_gcs "
      <> T.pack (show baselineSnapshot.cacheMemorySnapshotMajorGcs)

renderSnapshotDelta :: (Core.CacheMemorySnapshot -> Word64) -> Core.CacheMemorySnapshot -> Core.CacheMemorySnapshot -> T.Text
renderSnapshotDelta projection after before =
  renderSignedBytes (toInteger (projection after) - toInteger (projection before))

renderSample :: Core.CacheMemoryStats -> LoreDoc
renderSample sample =
  paragraph $
    sample.cacheMemoryStatsName
      <> ": live "
      <> renderBytes sample.cacheMemoryStatsBeforeLiveBytes
      <> " -> "
      <> renderBytes sample.cacheMemoryStatsAfterLiveBytes
      <> " ("
      <> renderSignedBytes sample.cacheMemoryStatsLiveBytesDelta
      <> "), mem_in_use "
      <> renderBytes sample.cacheMemoryStatsBeforeMemInUseBytes
      <> " -> "
      <> renderBytes sample.cacheMemoryStatsAfterMemInUseBytes
      <> " ("
      <> renderSignedBytes sample.cacheMemoryStatsMemInUseBytesDelta
      <> "), major_gcs "
      <> T.pack (show sample.cacheMemoryStatsBeforeMajorGcs)
      <> " -> "
      <> T.pack (show sample.cacheMemoryStatsAfterMajorGcs)

cacheMemInUseBytesFreed :: Core.CacheMemoryStats -> Integer
cacheMemInUseBytesFreed sample =
  negate sample.cacheMemoryStatsMemInUseBytesDelta

renderBytes :: (Integral a) => a -> T.Text
renderBytes bytes =
  T.pack (showFFloat (Just 2) mebibytes " MiB")
  where
    mebibytes :: Double
    mebibytes = fromIntegral bytes / 1_048_576

renderSignedBytes :: Integer -> T.Text
renderSignedBytes deltaBytes
  | deltaBytes < 0 = "-" <> renderBytes (abs deltaBytes)
  | otherwise = "+" <> renderBytes deltaBytes

renderSeconds :: Int -> T.Text
renderSeconds micros =
  T.pack (showFFloat (Just 2) seconds "s")
  where
    seconds :: Double
    seconds = fromIntegral micros / 1_000_000
