module FunctionsForSigs where

i :: a -> a
i x = x 

c :: a -> b -> a
c x y = x 

r:: [a] -> [a]
r x = x ++ x 

co :: (b -> c) -> (a -> b) -> a -> c
co bToc aTob a = (bToc . aTob) a