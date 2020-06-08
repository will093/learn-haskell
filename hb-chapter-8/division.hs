module Division where

data DividedResult =
    Result Integer
  | DividedByZero
  deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy x 0 = DividedByZero
dividedBy x y = Result (quotient x y)
  where quotient x y
          | (abs x) < (abs y) = 0
          | (x > 0) /= (y > 0) = (-1) + quotient (x + y) y
          | otherwise = 1 + quotient (x - y) y