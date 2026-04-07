module Demo
  ( lookupOrZero,
    lookupOrOne,
    lookupWithWhere,
    isTrue,
    explicitQualified,
    pairLeft,
    pairRight,
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

isTrue :: String -> Bool
isTrue "True" = True
isTrue "False" = False
isTrue _ = False

explicitQualified :: Char -> Bool
explicitQualified ch =
  Set.member ch (Set.fromList "abc")

pairLeft, pairRight :: Int
(pairLeft, pairRight) =
  ( fromMaybe 0 (Map.lookup "left" table),
    Map.size table
  )
  where
    table = Map.fromList [("left", 1), ("right", 2)]

type NameSet = Set.Set String

type family Elem (container :: Type) :: Type

data family Bucket (item :: Type) :: Type

data Indexed a = Indexed
  { indexedNames :: NameSet,
    indexedValues :: Map.Map String a
  }

class HasIndex a where
  toIndex :: a -> Map.Map String a
