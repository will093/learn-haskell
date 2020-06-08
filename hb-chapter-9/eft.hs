module EFT where

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x > y = []
  | x == y = [y]
  | otherwise = [x] ++ eftBool (succ x) y

eft :: (Ord a, Enum a) => a -> a -> [a]
eft x y
  | x > y = []
  | x == y = [y]
  | otherwise = [x] ++ eft (succ x) y