module Main where

import Lexer
import Model

main :: IO ()
main = do
  let input = "go -> take, mark, nothing."
  print $ alexScanTokens input