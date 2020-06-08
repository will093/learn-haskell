module Composition where

negateSum :: [Integer] -> Integer
negateSum = negate . sum

sumNegate :: [Integer] -> Integer
sumNegate = sum . map negate
