module Euler.E3Spec (spec) where

import Test.Hspec
import Data.Numbers.Primes

spec :: Spec
spec =
    describe "solution" $
        it "last (primeFactors 600851475143)" $
            last (primeFactors 600851475143) `shouldBe` (6857 :: Integer)
