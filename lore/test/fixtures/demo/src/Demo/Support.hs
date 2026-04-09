module Demo.Support
  ( supportSeed,
    supportStep,
    SupportRecord,
    mkSupportRecord,
    (.+.),
  )
where

import qualified Data.Map.Strict as Map

supportSeed :: Int
supportSeed = 5

supportStep :: Int -> Int
supportStep value = value + supportSeed

data SupportRecord = SupportRecord
  { supportValues :: Map.Map String Int
  }

mkSupportRecord :: Int -> SupportRecord
mkSupportRecord value =
  SupportRecord
    { supportValues = Map.singleton "value" value
    }

(.+.) :: Int -> Int -> Int
left .+. right = left + right
