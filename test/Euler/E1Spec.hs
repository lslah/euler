module Euler.E1Spec (spec) where

import Test.Hspec
import Euler.E1

spec :: Spec
spec = do
    describe "multiples" $
        it "lists ints which are multiples of any of the given ints" $
            takeWhile (<10) (multiples [3,5]) `shouldBe` [3, 5, 6, 9]
    describe "solution" $
        it "sum $ takeWhile (<1000) (multiples [3,5])" $
            sum (takeWhile (<1000) (multiples [3,5])) `shouldBe` 233168


