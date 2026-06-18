module Main
  ( main,
  )
where

import qualified ShellWordsSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    ShellWordsSpec.spec
