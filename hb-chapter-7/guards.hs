module Guards where

bloodNa :: Integer -> [Char]
bloodNa x
  | x < 135 = "Too Low"
  | x > 145 = "Too High"
  | otherwise = "Just Right"

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100