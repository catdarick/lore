module Fixture.Small.Records
  ( supportSeed,
    supportStep,
    SupportRecord (..),
    mkSupportRecord,
    commonRun,
  )
where

import qualified Data.Map.Strict as Map

supportSeed :: Int
supportSeed = 5

supportStep :: Int -> Int
supportStep value = value + supportSeed

data SupportRecord = SupportRecord
  { supportValues :: Map.Map String Int,
    supportMeta :: String
  }

mkSupportRecord :: Int -> SupportRecord
mkSupportRecord value =
  SupportRecord
    { supportValues = Map.singleton "value" value,
      supportMeta = "seed:" <> show supportSeed
    }

commonRun :: Int -> Int
commonRun value = value + supportSeed
