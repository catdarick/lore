module Fixture.Small.CommonNames.B
  ( run,
    mapValue,
    build,
    update,
  )
where

run :: Int -> Int
run value = value + 2

mapValue :: (a -> b) -> [a] -> [b]
mapValue = map

build :: Int -> [Int]
build n = [n, n + 1]

update :: Int -> Int
update value = value - 2
