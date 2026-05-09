module Lore.Internal.Cache.Types
  ( CacheLookup (..),
  )
where

data CacheLookup a
  = CacheMiss
  | CacheHit a
