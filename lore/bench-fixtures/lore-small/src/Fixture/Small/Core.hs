module Fixture.Small.Core
  ( lookupOrZero,
    lookupOrOne,
    lookupWithWhere,
    explicitQualified,
    crossModuleRecord,
    crossModuleSeed,
    crossModuleBundle,
    seedValue,
    bumpWithSeed,
    derivedValue,
    mkIndexed,
    commonRun,
    NameSet,
    Elem,
    Bucket,
    Indexed (..),
    HasIndex (..),
  )
where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (Set, fromList, member)
import qualified Fixture.Small.Records as Records

lookupOrZero :: [(String, Int)] -> String -> Int
lookupOrZero pairs key =
  fromMaybe 0 (Map.lookup key (Map.fromList pairs))

lookupOrOne :: [(String, Int)] -> String -> Int
lookupOrOne pairs key =
  fromMaybe 1 (Map.lookup key (Map.fromList pairs))

lookupWithWhere :: [(String, Int)] -> String -> Int
lookupWithWhere pairs key =
  fromMaybe fallback (Map.lookup key table)
  where
    table = Map.fromList pairs
    fallback = Map.size table

explicitQualified :: Char -> Bool
explicitQualified ch =
  Set.member ch (Set.fromList "abc")

crossModuleRecord :: Int -> Records.SupportRecord
crossModuleRecord value =
  Records.mkSupportRecord (Records.supportStep value)

crossModuleSeed :: Int
crossModuleSeed = Records.supportSeed

crossModuleBundle :: Int -> (Int, Records.SupportRecord)
crossModuleBundle value =
  (crossModuleSeed, crossModuleRecord value)

seedValue :: Int
seedValue = 40

bumpWithSeed :: Int -> Int
bumpWithSeed value = value + seedValue

derivedValue :: Int
derivedValue = bumpWithSeed 2

mkIndexed :: NameSet -> Indexed Int
mkIndexed names =
  Indexed
    { indexedNames = names,
      indexedValues = Map.empty
    }

commonRun :: Int -> Int
commonRun value = value + 1

type NameSet = Set.Set String

type family Elem (container :: Type) :: Type

data family Bucket (item :: Type) :: Type

data Indexed a = Indexed
  { indexedNames :: NameSet,
    indexedValues :: Map.Map String a
  }

class HasIndex a where
  toIndex :: a -> Map.Map String a
