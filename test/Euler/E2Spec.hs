module Euler.E2Spec (spec) where

import Test.Hspec
import Euler.E2

spec :: Spec
spec = do
    describe "fib" $
        it "[1,2,3,5,8..]" $
            take 5 fib `shouldBe` [1, 2, 3, 5, 8]
    describe "every n" $
        it "takes only every nth element from a list" $
            every 3 [1,2,3,4,5,6,7,8] `shouldBe` ([1,4,7] :: [Int])
    describe "solution" $
        it "sum . takeWhile (<=4000000) . every 3 . drop 1 $ fib" $
            (sum . takeWhile (<=4000000) . every 3 . drop 1 $ fib) `shouldBe` 4613732
