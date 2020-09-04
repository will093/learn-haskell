module FunctorExercises where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x) 

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


type IntToInt = Fun Int Int
type PairIntFC = Pair Int -> IntToInt -> IntToInt -> Bool

type PairIntFI = Pair Int -> Bool

data Pair a = Pair a a
  deriving (Eq, Show)

genPair :: (Arbitrary a) => Gen (Pair a)
genPair = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = genPair

  

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b) 


main :: IO ()
main = do
  quickCheck (functorIdentity :: PairIntFI)
  quickCheck (functorCompose' :: PairIntFC)