module Print1 where

myGreeting:: String
myGreeting = "hello " ++ "world"

hello :: [Char]
hello = "hello"

world :: String
world = "world"

main :: IO()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = hello ++  " " ++ world