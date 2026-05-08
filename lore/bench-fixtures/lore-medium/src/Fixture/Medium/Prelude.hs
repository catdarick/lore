module Fixture.Medium.Prelude
  ( sharedRun,
    sharedBuild,
    sharedUpdate,
  )
where

sharedRun :: Int -> Int
sharedRun value = value + 1

sharedBuild :: Int -> [Int]
sharedBuild n = [1 .. n]

sharedUpdate :: Int -> Int
sharedUpdate value = value * 3
