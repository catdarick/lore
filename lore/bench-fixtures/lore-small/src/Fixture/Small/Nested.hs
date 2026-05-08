module Fixture.Small.Nested
  ( nestedDecision,
    commonRun,
  )
where

import qualified Data.Map.Strict as Map

nestedDecision :: [(String, Int)] -> Int
nestedDecision pairs =
  let table = Map.fromList pairs
      step key fallback =
        case Map.lookup key table of
          Just value
            | value > 10 ->
                if value `mod` 2 == 0
                  then value + Map.size table
                  else value - 1
            | otherwise -> fallback
          Nothing -> fallback
   in step "x" (step "y" 0)

commonRun :: Int -> Int
commonRun value = value - 1
