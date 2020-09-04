module MonadExamples where

import Data.Maybe(catMaybes)

-- Simple examples

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = 
  xs >>= (\x -> if even x then [x*x, x*x] else [x*x])

maybeProcess :: Maybe Integer -> Maybe Integer
maybeProcess x = x >>= (\x -> Just 7)

-- Cow example
data Cow = Cow {
  name :: String,
  age :: Int,
  weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative x 
  | x < 0 = Nothing
  | otherwise = Just x

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c

-- With Monad syntax
-- mkSphericalCow :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow name' age' weight' =
--   noEmpty name' >>= \nammy -> noNegative age' >>= \agey -> noNegative weight' >>= \weighty -> weightCheck (Cow nammy agey weighty)

-- With do syntax
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)