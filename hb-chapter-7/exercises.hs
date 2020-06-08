module Exercises where

-- nthDigit :: a -> a -> a
nthDigit n x = x `div` (10 ^ (((length . show) x) - n)) `mod` 10