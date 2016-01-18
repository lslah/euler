module Euler.E60
    ( concatInts
    , remarkable
    , remarkableSet
    , addToRemarkableSet
    , solve
    , s
    )
    where

import Data.Monoid
import Data.Maybe
import Data.Numbers.Primes

solve :: Int -> Int
solve n = sum $ head $ dropWhile (\xs -> length xs < n) s

s :: [[Int]]
s = [] : concatMap (\p -> mapMaybe (addToRemarkableSet p) (takeWhile (f p) s)) primes
    where
        f _ [] = True
        f y (x:_) = x < y

addToRemarkableSet :: Int -> [Int] -> Maybe [Int]
addToRemarkableSet y [] = Just [y]
addToRemarkableSet y (x:xs)
    | remarkableSet (x:xs) y = Just (y:x:xs)
    | otherwise = Nothing

remarkableSet :: [Int] -> Int -> Bool
remarkableSet xs y = all (== True) $ map (remarkable y) xs

remarkable :: Int -> Int -> Bool
remarkable x y = {-# SCC remarkable #-} isPrime (concatInts x y) && isPrime (concatInts y x)

concatInts :: Int -> Int -> Int
concatInts x y
    | x <= 0 || y <= 0 = error "Invalid arguments"
    | otherwise = read $ show x <> show y
