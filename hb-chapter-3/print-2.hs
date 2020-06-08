-- print2 module
module Print2 where

greeting :: String
greeting = "Yarrr"

printSecond :: IO ()
printSecond = putStrLn aGreeting
  where aGreeting = "Yarrr"

main :: IO () 
main = do
  putStrLn greeting
  printSecond




