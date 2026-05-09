module Lore.Mcp.Internal.List
  ( minimumMaybe,
    maximumMaybe,
  )
where

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe = \case
  [] -> Nothing
  values -> Just (minimum values)

maximumMaybe :: (Ord a) => [a] -> Maybe a
maximumMaybe = \case
  [] -> Nothing
  values -> Just (maximum values)
