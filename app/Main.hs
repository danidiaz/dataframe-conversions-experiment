module Main where

import qualified Frame (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Frame.someFunc
