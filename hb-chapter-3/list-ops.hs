module ListOps where

thirdLetter :: String -> Char
thirdLetter x = x !! 2 

letterIndex :: Int -> Char
letterIndex x = curry !! x
  where curry = "Curry is awesome!"

rvrs :: String
rvrs = (drop 9 x) ++ " " ++ (drop 6 $ take 8 x) ++ " " ++ (take 5 x)
  where x = "Curry is awesome!"