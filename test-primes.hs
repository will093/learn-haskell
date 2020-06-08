module TestPrimes where

primes :: [Integer]
primes = sieve [2..] 
  where
    sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p > 0 ]