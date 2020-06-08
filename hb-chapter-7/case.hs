module Case where

isPalindrome :: [Char] -> [Char]
isPalindrome xs = case xs == reverse xs of
  True -> "Yes"
  False -> "No"