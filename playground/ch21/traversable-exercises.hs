module TraversableExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

-- Identity 
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> (f a)

genIdentity :: (Arbitrary a) => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = genIdentity

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-- Maybe
data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ (Nada) = pure Nada
  traverse f (Yep a) = Yep <$> (f a)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [ (1, return Nada)
                        , (3, liftM Yep arbitrary) ]

instance (Eq a) => EqProp (Optional a) where (=-=) = eq

main :: IO ()
main = do
  let trigger1 = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable trigger1)
  let trigger2 = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable trigger2)

