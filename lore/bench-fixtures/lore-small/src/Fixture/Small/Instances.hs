module Fixture.Small.Instances
  ( renderInt,
    commonRun,
  )
where

import qualified Data.Map.Strict as Map
import qualified Fixture.Small.Records as Records

class Render a where
  render :: a -> String

instance Render Int where
  render value = "int:" <> show value

instance Render Bool where
  render value = if value then "true" else "false"

instance Render Records.SupportRecord where
  render record = show (Map.size record.supportValues)

renderInt :: Int -> String
renderInt = render

commonRun :: Int -> Int
commonRun value = value + 2
