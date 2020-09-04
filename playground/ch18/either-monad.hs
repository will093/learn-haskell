module EitherMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a 
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First f <*> _ = First f
  _ <*> First a = First a
  Second f <*> Second b = Second (f b)

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second a >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = 
    frequency [ (1, liftM First arbitrary)
              , (3, liftM Second arbitrary) ]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq


main = do
  let trigger = undefined :: Sum Int (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger