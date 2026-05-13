module Lore.Internal.SourceSpan.Types
  ( Span (..),
  )
where

data Span = Span
  { spanFile :: FilePath,
    spanStartLine :: Int,
    spanStartCol :: Int,
    spanEndLine :: Int,
    spanEndCol :: Int
  }
  deriving (Eq, Show)
