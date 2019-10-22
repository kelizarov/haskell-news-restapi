module Main
  ( main
  )
where

import qualified RouterSpec                    as RS
import qualified UseCases.RegisterUserSpec     as RUS
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  spec <- testSpec "RegisterUser spec" RUS.spec
  defaultMain (testGroup "main tests" [spec])
