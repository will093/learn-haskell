module GreetIfCool where

greetIfCool :: String -> IO ()
greetIfCool coolness = if cool
  then putStrLn "Sup"
  else putStrLn ":("
  where cool = coolness == "meow" let x = 5
  x + 3 == 5
  