module QuickCheckExercises where

import Test.Hspec
import Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

testHalf :: IO ()
testHalf = hspec $ do
  describe "half" $ do
    it "should half a positive number" $ do
      half 5 `shouldBe` 2.5
    it "should compose with *2 to form the identity function." $ do
      property $ \x -> ((*2) . half) x == (x :: Double)

testAddition :: IO ()
testAddition = hspec $ do
  describe "addition" $ do
    it "should be commutative" $ do
      property $ \x y z -> x + (y + z) == ((x :: Int) + (y :: Int)) + (z :: Int)[]
    it "should be associative" $ do
      property $ \x y -> x + y == ((y :: Int) + (x :: Int))



