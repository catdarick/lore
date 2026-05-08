module Fixture.Medium.Module001
  ( run,
    mapValue,
    build,
    update,
  )
where

import Fixture.Medium.Core (coreRun)
import Fixture.Medium.Prelude

run :: Int -> Int
run value = sharedRun (coreRun value)

mapValue :: (a -> b) -> [a] -> [b]
mapValue = map

build :: Int -> [Int]
build value = sharedBuild value

update :: Int -> Int
update value = sharedUpdate value
