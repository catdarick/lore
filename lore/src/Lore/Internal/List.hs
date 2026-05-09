module Lore.Internal.List
  ( maybeToList,
    mapMaybeToList,
    minimumMaybe,
    maximumMaybe,
    firstJust,
    dedupeOn,
  )
where

import qualified Data.Set as Set

maybeToList :: Maybe a -> [a]
maybeToList = \case
  Just value -> [value]
  Nothing -> []

mapMaybeToList :: (a -> Maybe b) -> [a] -> [b]
mapMaybeToList f =
  foldr
    (\value acc -> maybe acc (: acc) (f value))
    []

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe = \case
  [] -> Nothing
  values -> Just (minimum values)

maximumMaybe :: (Ord a) => [a] -> Maybe a
maximumMaybe = \case
  [] -> Nothing
  values -> Just (maximum values)

firstJust :: [Maybe a] -> Maybe a
firstJust = \case
  [] -> Nothing
  Just value : _ -> Just value
  Nothing : rest -> firstJust rest

dedupeOn :: (Ord b) => (a -> b) -> [a] -> [a]
dedupeOn keyOf =
  reverse . snd . foldl go (Set.empty, [])
  where
    go (seen, values) value
      | key `Set.member` seen =
          (seen, values)
      | otherwise =
          (Set.insert key seen, value : values)
      where
        key = keyOf value
