{-# LANGUAGE OverloadedStrings #-}
module Main where

import Frame
import Data.Function ((&))

main :: IO ()
main = do
  frame <- createExampleFrame
  putStrLn "Initial frame:"
  frame & describe
  frame & "foo" .= "inty" +: "floaty"
  putStrLn "Frame after op:"
  frame & describe
  putStrLn "The foo col:"
  frame & printCol "foo"