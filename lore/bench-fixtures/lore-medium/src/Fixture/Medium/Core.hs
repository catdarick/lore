module Fixture.Medium.Core
  ( coreRun,
    coreBuild,
    coreUpdate,
  )
where

import Fixture.Medium.Prelude

coreRun :: Int -> Int
coreRun = sharedRun

coreBuild :: Int -> [Int]
coreBuild = sharedBuild

coreUpdate :: Int -> Int
coreUpdate = sharedUpdate
