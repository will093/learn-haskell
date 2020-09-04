module Pg where

import Data.List

lexiPos :: String -> Integer
lexiPos [x] = 1 
lexiPos word@(x:xs) = (foldr (\x acc -> acc + (getPermutationCount $ delete x word)) 0 lessThanX) + (lexiPos xs)
  where
    -- Unique chars in the string which precede x alphabetically
    lessThanX = nub $ filter (<x) xs

-- Return the total number of permutations of a given string
getPermutationCount :: String -> Integer
getPermutationCount xs = (factorial . length) xs `div` (foldr ((*) . factorial . length) 1 grouped)
  where
    grouped = (group . sort) xs
    factorial :: Int -> Integer
    factorial n = product [1,2..(toInteger n)]