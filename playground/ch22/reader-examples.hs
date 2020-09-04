module ReaderExamples where

import Control.Monad
import Control.Monad.Reader

greeter :: Reader String String
greeter = do
    name <- ask
    return ("hello, " ++ name ++ "!")

main :: String
main = runReader greeter $ "meow" 