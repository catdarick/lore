module Fixture.Medium.Module076
  ( run,
    mapValue,
    build,
    update,
  )
where

import qualified Fixture.Medium.Module075 as Prev
import Fixture.Medium.Prelude

run :: Int -> Int
run value = sharedRun (Prev.run value)

mapValue :: (a -> b) -> [a] -> [b]
mapValue = map

build :: Int -> [Int]
build value = sharedBuild value

update :: Int -> Int
update value = sharedUpdate value
