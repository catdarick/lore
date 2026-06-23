module Lore.Tools.Render.Units
  ( renderBytes,
    renderSignedBytes,
    renderMicrosAsSeconds,
    renderNanosecondsAsSeconds,
  )
where

import qualified Data.Text as T
import Numeric (showFFloat)

renderBytes :: (Integral a) => a -> T.Text
renderBytes bytes =
  T.pack (showFFloat (Just 2) mebibytes " MiB")
  where
    mebibytes :: Double
    mebibytes = fromIntegral bytes / 1_048_576

renderSignedBytes :: Integer -> T.Text
renderSignedBytes deltaBytes
  | deltaBytes < 0 = "-" <> renderBytes (abs deltaBytes)
  | otherwise = "+" <> renderBytes deltaBytes

renderMicrosAsSeconds :: (Integral a) => a -> T.Text
renderMicrosAsSeconds micros =
  T.pack (showFFloat (Just 2) seconds "s")
  where
    seconds :: Double
    seconds = fromIntegral micros / 1_000_000

renderNanosecondsAsSeconds :: (Integral a) => a -> T.Text
renderNanosecondsAsSeconds nanos =
  T.pack (showFFloat (Just 2) seconds "s")
  where
    seconds :: Double
    seconds = fromIntegral nanos / 1_000_000_000
