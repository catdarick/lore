module Main
  ( main,
  )
where

import qualified GetDefinitionSpec
import qualified ProtocolRequestSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    GetDefinitionSpec.spec
    ProtocolRequestSpec.spec
