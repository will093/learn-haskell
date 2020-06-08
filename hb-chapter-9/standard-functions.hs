module StandardFunctions where

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs 

squishMap :: (a -> [b]) -> [a] -> [b]s
squishMap f = squish . map f