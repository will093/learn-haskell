{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module NewType where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (i, s) = i > 42

instance TooMany (Int, Int) where
  tooMany (i1, i2) = i1 + i2 > 42

-- instance TooMany (Num a) => (a, a) where
--   tooMany (n1, n2) = n1 + n2 > 42
