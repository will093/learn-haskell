module EqInstances where

-- example 1 

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn integer) (TisAn integer') =
    integer == integer'

-- example 2

data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int) (TisAString string) = False
  (==) (TisAnInt int) (TisAnInt int') = int == int'
  (==) (TisAString string) (TisAString string') = string == string'max 4 5
  