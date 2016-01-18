module Euler.E60Spec (main, spec) where

import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Control.Exception

import Euler.E60

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "solve" $ do
        it "example" $
            solve 4 `shouldBe` 3+7+109+673
        it "solution" $
            solve 5 `shouldBe` 0
    describe "remarkableSet" $ do
        it "two ints form a remarkable set if they are remarkable" $ property $
            \(Positive x) (Positive y) -> remarkable x y == remarkableSet [x] y
        it "examples" $ do
            remarkableSet [3, 7, 109] 673 `shouldBe` True
            remarkableSet [3, 7, 109] 5 `shouldBe` False
    describe "addToRemarkableSet" $ do
        it "two ints form a remarkable set if they are remarkable" $ property $
            \(Positive x) (Positive y) -> remarkable x y == isJust (addToRemarkableSet y [x])
        it "examples" $ do
            addToRemarkableSet 673 [3, 7, 109] `shouldBe` Just [673, 3, 7, 109]
            addToRemarkableSet 5 [3, 7, 109] `shouldBe` Nothing
    describe "remarkable" $ do
        it "is symmetric" $ property $
            \(Positive x) (Positive y) -> remarkable x y == remarkable y x
        it "identifies remarkable ints" $ do
            remarkable 3 109 `shouldBe` True
            remarkable 7 673 `shouldBe` True
        it "identifies unremarkable ints" $ do
            remarkable 3 5 `shouldBe` False
            remarkable 5 673 `shouldBe` False
    describe "concatInts" $ do
        it "concatenates two positive integers" $ do
            concatInts 1 2 `shouldBe` 12
            concatInts 13 531 `shouldBe` 13531
        it "fails for non-positive integers" $ property $
            \(NonNegative x) ->
                evaluate (concatInts 42 (-x)) `shouldThrow` errorCall "Invalid arguments"
