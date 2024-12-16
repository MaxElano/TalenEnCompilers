module Interpreter where

import ParseLib

import Data.Map (Map)
import qualified Data.Map as L

import Data.Char (isSpace)
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra
import Data.List


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving Eq

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace space = concatMap printSpaceLine cs
  where
    spaceKeys = L.keys space
    (_,width) = spaceKeys !! (length spaceKeys - 1)
    spaceValues = L.elems space
    cs = splitSpace width spaceValues

printSpaceLine :: [Contents] -> String
printSpaceLine space = concatMap handleChar space ++ "\r\n"
  where
    handleChar c = maybe "" (\(_,char) -> [char]) (findContent c)

findContent :: Contents -> Maybe (Contents, Char)
findContent c1 = find (\(c2,char) -> c1 == c2) contentsTable

splitSpace :: Int -> [Contents] -> [[Contents]]
splitSpace _ [] = []
splitSpace n cs = let (splt1,splt2) = splitAt n cs
                  in splt1 : splitSpace n splt2

-- These three should be defined by you
type Ident = ()
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment = undefined

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


