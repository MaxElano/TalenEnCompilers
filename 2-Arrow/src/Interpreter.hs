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
import Data.Maybe as M


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Show, Eq)

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
    (_,width) = fst $  L.findMax space -- Finds the width of a row in the space
    spaceValues = L.elems space
    cs = splitSpace (width+1) spaceValues -- Returns a list of lists which contains the contents of each row respectively

printSpaceLine :: [Contents] -> String
printSpaceLine space = concatMap handleChar space ++ "\r\n"
  where
    handleChar c = maybe "" (:[]) (lookup c contentsTable)

splitSpace :: Int -> [Contents] -> [[Contents]]
splitSpace _ [] = []
splitSpace n cs = let (splt1,splt2) = splitAt n cs
                  in splt1 : splitSpace n splt2

-- These three should be defined by you
type Ident = String
type Commands = [Command]
type Heading = Orientation

data Orientation = North | East | South | West deriving Show

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- Exercise 8
toEnvironment :: String -> Environment
toEnvironment s = if check then L.fromList $ preProcessProgram program else L.empty
  where
    tokens = alexScanTokens s
    program = parser tokens
    check = checkProgram program

preProcessProgram :: Program -> [(Ident,Commands)]
preProcessProgram = map (\(Rule a b) -> (a,b)) -- Transforming from rules to pairs makes it easier to turn into a Data.map (dictionary)

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step _ (ArrowState space pos heading []) = Done space pos heading
step env arrow@(ArrowState space pos heading (head:stack)) = case head of -- Correctly reacts to each of the possible types of commands, often using a seperate function defined below
  ComGo                    -> Ok $ ArrowState space (moveArrow space pos heading) heading stack
  ComTake                  -> Ok $ ArrowState (takeItem space pos) pos heading stack
  ComMark                  -> Ok $ ArrowState (L.insert pos Lambda space) pos heading stack
  ComNothing               -> Ok $ ArrowState space pos heading stack
  (ComTurn direction)      -> Ok $ ArrowState space pos (turnArrow heading direction) stack
  (ComCase direction alts) -> caseArrow (ArrowState space pos heading stack) (getNextPos pos $ turnArrow heading direction) alts 
  (ComIdent ident)         -> maybe (Fail "Error: No rule matched.") (\coms -> Ok $ ArrowState space pos heading (coms ++ stack)) (L.lookup ident env) -- Returns error state if no ruleset is found using the ident string

moveArrow :: Space -> Pos -> Heading -> Pos
moveArrow space pos heading =
  let newPos = getNextPos pos heading
  in maybe pos (\c -> if c == Empty || c == Lambda || c == Debris then newPos else pos) $ L.lookup newPos space

takeItem :: Space -> Pos -> Space
takeItem space pos = maybe space (\c -> if c == Lambda || c == Debris then L.insert pos Empty space else space) (L.lookup pos space)

turnArrow :: Heading -> Direction -> Heading
turnArrow heading DirFront = heading
turnArrow heading DirLeft  = turnLeft heading
turnArrow heading DirRight = turnRight heading

caseArrow :: ArrowState -> Pos -> [Alt] -> Step
caseArrow arrow@(ArrowState space pos heading stack) newPos alts =
  let content  = M.fromMaybe Boundary (L.lookup newPos space) -- Assumes it is a boundary if nothing is found
  in maybe (Fail "Error: No alternative matched.") (\(Alt _ coms) -> Ok $ ArrowState space pos heading (coms ++ stack)) (find (\(Alt pattern _) -> eqContentPattern content pattern) alts) 
  -- Looks for a match in the alts and if no matches are found returns error state, else returns the arrowstate with new found commands prepended

getNextPos :: Pos -> Heading -> Pos
getNextPos (row,column) North = (row-1,column)
getNextPos (row,column) East  = (row,column+1)
getNextPos (row,column) South = (row+1,column)
getNextPos (row,column) West  = (row,column-1)

turnLeft :: Heading -> Heading
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnRight :: Heading -> Heading
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

eqContentPattern :: Contents -> Pattern -> Bool
eqContentPattern Empty PatEmpty       = True
eqContentPattern Lambda PatLambda     = True
eqContentPattern Debris PatDebris     = True
eqContentPattern Asteroid PatAsteroid = True
eqContentPattern Boundary PatBoundary = True
eqContentPattern _ PatWildcard        = True
eqContentPattern _ _                  = False