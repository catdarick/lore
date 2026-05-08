module Fixture.Small.CommonNames.C
  ( run,
    mapValue,
    build,
    update,
  )
where

run :: Int -> Int
run value = value + 3

mapValue :: (a -> b) -> [a] -> [b]
mapValue = map

build :: Int -> [Int]
build n = [n * 2]

update :: Int -> Int
update value = value + 100
