module Main
  ( main
  ) where

import qualified RouterSpec as RS
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  spec <- testSpec "Router Tests" RS.spec
  defaultMain (testGroup "main tests" [spec])
