module Euler.E2
    ( fib
    , every
    )
    where

fib :: [Int]
fib = scanl (+) 1 (1:fib)

every :: Int -> [a] -> [a]
every _ [] = []
every n (x:xs) = x : every n (drop (n-1) xs)

