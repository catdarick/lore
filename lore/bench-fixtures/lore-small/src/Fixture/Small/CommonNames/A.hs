module Fixture.Small.CommonNames.A
  ( run,
    mapValue,
    build,
    update,
  )
where

run :: Int -> Int
run value = value + 1

mapValue :: (a -> b) -> [a] -> [b]
mapValue = map

build :: Int -> [Int]
build n = [1 .. n]

update :: Int -> Int
update value = value * 2
