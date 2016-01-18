module Euler.E1
    ( multiples
    )
    where

multiples :: [Int] -> [Int]
multiples xs = [x | x <- [1..], multiple x]
    where
        multiple x = any (\y -> x `mod` y == 0) xs
