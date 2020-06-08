module Exercises where

import Data.Maybe
import Data.Char

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

notThe :: String -> Maybe String
notThe w 
  | w == "the" = Nothing
  | otherwise = Just w


newtype Word' = Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word
  | (foldl (\t c -> if elem (toLower c) vowels then t + 1 else t) 0 word) <= length word `div` 2 = Just (Word' word)
  | otherwise = Nothing  