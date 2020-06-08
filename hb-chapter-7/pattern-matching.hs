module PatternMatching where

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))


-- RegisteredUser example

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name) (AccountNumber acctNum))
  = putStrLn $ name ++ " " ++ show acctNum