module MyZip where

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] ys = []
myZipWith _ xs [] = [] 
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)