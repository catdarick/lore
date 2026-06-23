module Lore.Tools.RtsStats
  ( RtsStatsOutput (..),
    rtsStats,
    renderRtsStats,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified GHC.Stats as RTS
import Lore.Tools.Render.Doc
  ( LoreDoc,
    bulletList,
    heading2,
    heading3,
    paragraph,
  )
import Lore.Tools.Render.Units
  ( renderBytes,
    renderNanosecondsAsSeconds,
  )

data RtsStatsOutput
  = RtsStatsDisabled
  | RtsStatsEnabled RTS.RTSStats

rtsStats :: (MonadIO m) => m RtsStatsOutput
rtsStats = do
  statsEnabled <- liftIO RTS.getRTSStatsEnabled
  if statsEnabled
    then RtsStatsEnabled <$> liftIO RTS.getRTSStats
    else pure RtsStatsDisabled

renderRtsStats :: RtsStatsOutput -> LoreDoc
renderRtsStats RtsStatsDisabled =
  heading2 "RTS Stats"
    <> paragraph "RTS stats are disabled. Re-run with +RTS -T -RTS, or use a binary built with -with-rtsopts=-T."
renderRtsStats (RtsStatsEnabled stats) =
  heading2 "RTS Stats"
    <> renderCounts stats
    <> heading3 "Allocation and Residency"
    <> bulletList (allocationStats stats)
    <> heading3 "Memory in Use"
    <> bulletList (memoryStats stats)
    <> heading3 "CPU and Elapsed Time"
    <> bulletList (timeStats stats)
    <> heading3 "Most Recent GC"
    <> bulletList (gcDetailsStats (RTS.gc stats))

renderCounts :: RTS.RTSStats -> LoreDoc
renderCounts stats =
  paragraph $
    "GCs: total "
      <> renderIntegral (RTS.gcs stats)
      <> ", major "
      <> renderIntegral (RTS.major_gcs stats)

allocationStats :: RTS.RTSStats -> [LoreDoc]
allocationStats stats =
  [ statLine "allocated_bytes" (renderBytes (RTS.allocated_bytes stats)),
    statLine "copied_bytes" (renderBytes (RTS.copied_bytes stats)),
    statLine "max_live_bytes" (renderBytes (RTS.max_live_bytes stats)),
    statLine "cumulative_live_bytes" (renderBytes (RTS.cumulative_live_bytes stats)),
    statLine "max_large_objects_bytes" (renderBytes (RTS.max_large_objects_bytes stats)),
    statLine "max_compact_bytes" (renderBytes (RTS.max_compact_bytes stats)),
    statLine "max_slop_bytes" (renderBytes (RTS.max_slop_bytes stats))
  ]

memoryStats :: RTS.RTSStats -> [LoreDoc]
memoryStats stats =
  [ statLine "max_mem_in_use_bytes" (renderBytes (RTS.max_mem_in_use_bytes stats)),
    statLine "par_copied_bytes" (renderBytes (RTS.par_copied_bytes stats)),
    statLine "cumulative_par_max_copied_bytes" (renderBytes (RTS.cumulative_par_max_copied_bytes stats)),
    statLine "cumulative_par_balanced_copied_bytes" (renderBytes (RTS.cumulative_par_balanced_copied_bytes stats))
  ]

timeStats :: RTS.RTSStats -> [LoreDoc]
timeStats stats =
  [ statLine "init_cpu_ns" (renderNanosecondsAsSeconds (RTS.init_cpu_ns stats)),
    statLine "init_elapsed_ns" (renderNanosecondsAsSeconds (RTS.init_elapsed_ns stats)),
    statLine "mutator_cpu_ns" (renderNanosecondsAsSeconds (RTS.mutator_cpu_ns stats)),
    statLine "mutator_elapsed_ns" (renderNanosecondsAsSeconds (RTS.mutator_elapsed_ns stats)),
    statLine "gc_cpu_ns" (renderNanosecondsAsSeconds (RTS.gc_cpu_ns stats)),
    statLine "gc_elapsed_ns" (renderNanosecondsAsSeconds (RTS.gc_elapsed_ns stats)),
    statLine "cpu_ns" (renderNanosecondsAsSeconds (RTS.cpu_ns stats)),
    statLine "elapsed_ns" (renderNanosecondsAsSeconds (RTS.elapsed_ns stats))
  ]

gcDetailsStats :: RTS.GCDetails -> [LoreDoc]
gcDetailsStats details =
  [ statLine "gcdetails_gen" (renderIntegral (RTS.gcdetails_gen details)),
    statLine "gcdetails_threads" (renderIntegral (RTS.gcdetails_threads details)),
    statLine "gcdetails_allocated_bytes" (renderBytes (RTS.gcdetails_allocated_bytes details)),
    statLine "gcdetails_live_bytes" (renderBytes (RTS.gcdetails_live_bytes details)),
    statLine "gcdetails_large_objects_bytes" (renderBytes (RTS.gcdetails_large_objects_bytes details)),
    statLine "gcdetails_compact_bytes" (renderBytes (RTS.gcdetails_compact_bytes details)),
    statLine "gcdetails_slop_bytes" (renderBytes (RTS.gcdetails_slop_bytes details)),
    statLine "gcdetails_mem_in_use_bytes" (renderBytes (RTS.gcdetails_mem_in_use_bytes details)),
    statLine "gcdetails_copied_bytes" (renderBytes (RTS.gcdetails_copied_bytes details)),
    statLine "gcdetails_par_max_copied_bytes" (renderBytes (RTS.gcdetails_par_max_copied_bytes details)),
    statLine "gcdetails_par_balanced_copied_bytes" (renderBytes (RTS.gcdetails_par_balanced_copied_bytes details)),
    statLine "gcdetails_sync_elapsed_ns" (renderNanosecondsAsSeconds (RTS.gcdetails_sync_elapsed_ns details)),
    statLine "gcdetails_cpu_ns" (renderNanosecondsAsSeconds (RTS.gcdetails_cpu_ns details)),
    statLine "gcdetails_elapsed_ns" (renderNanosecondsAsSeconds (RTS.gcdetails_elapsed_ns details))
  ]

statLine :: T.Text -> T.Text -> LoreDoc
statLine label value =
  paragraph (label <> ": " <> value)

renderIntegral :: (Integral a) => a -> T.Text
renderIntegral =
  T.pack . show . toInteger
