module Fixture.Small.Qualified
  ( supportSummary,
    commonRun,
  )
where

import qualified Fixture.Small.Core as Core
import qualified Fixture.Small.Records as Records

supportSummary :: Int -> (Int, Records.SupportRecord)
supportSummary value =
  (Core.crossModuleSeed, Core.crossModuleRecord value)

commonRun :: Int -> Int
commonRun = Core.commonRun
