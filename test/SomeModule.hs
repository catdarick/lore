module SomeModule
  ( foo,
  )
where

foo :: Int -> Bool
foo = \case
  1 -> True
