module Main
    ( main
    )
where

import           Test.Tasty
import           Test.Tasty.Hspec
import qualified RouterSpec                    as RS

main :: IO ()
main = do
    spec <- testSpec "Router Tests" RS.spec
    defaultMain (testGroup "main tests" [spec])
