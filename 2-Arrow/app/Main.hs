--Names:
--Max Ferket 7337248
--Daniël van der Hoeven 5041503

module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser
import ParseLib ( parse )
import Data.Map
import Data.Maybe (listToMaybe)


-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env arrow = do
  let nextState = step env arrow
  case nextState of
    (Done space (row,column) heading) -> do
      putStrLn "-----------------------------------"
      putStrLn "\r\nThe program has terminated succesfully."
      putStrLn "\r\nFinal Space:"
      putStrLn (printSpace space)
      putStrLn ("Arrow position: (Row: " ++ show row ++ ", Column: " ++ show column ++ ")")
      putStrLn ("Arrow heading:  " ++ show heading)
      putStrLn "\r\nPress enter to exit program."
      input <- getLine
      return ()
    (Ok newArrow@(ArrowState space (row,column) heading stack)) -> do
      putStrLn "-----------------------------------"
      putStrLn "\r\nCurrent Space:"
      putStrLn (printSpace space)
      putStrLn ("Arrow position: (Row: " ++ show row ++ ", Column: " ++ show column ++ ")")
      putStrLn ("Arrow heading:  " ++ show heading)
      putStrLn ("Next command:   " ++ show (listToMaybe stack))
      putStrLn "\r\nPress any key to continue program or type 'quit' to exit program."
      input <- getLine
      if input == "quit" then return () else interactive env newArrow
    (Fail errorMessage) -> do
      putStrLn "-----------------------------------"
      putStrLn "\r\nThe program has terminated unsuccesfully."
      putStrLn errorMessage
      putStrLn "\r\nPress enter to exit program."
      input <- getLine
      return ()

-- Recursively runs the step function so long as it isn't done
batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env arrow@(ArrowState space pos heading _) = case step env arrow of
  (Done newSpace newPos newHeading) -> (newSpace, newPos, newHeading)
  (Ok newArrow) -> batch env newArrow
  (Fail text) -> (space, pos, heading)

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.

-- The main function is just a setup to running the interactive driver of the find program in the maze space
main :: IO ()
main = do
  findArrowString <- readFile "examples/Find.arrow"
  mazeSpaceString <- readFile "examples/Maze.space"
  let space = fst . head $ parse parseSpace mazeSpaceString
  let env = toEnvironment findArrowString
  -- This takes quit a while but shows that the program works as intended
  interactive env (ArrowState space (0,0) South [ComIdent "start"])