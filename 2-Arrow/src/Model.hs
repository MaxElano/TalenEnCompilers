module Model where

-- Exercise 1
data Token = Token deriving Show
  | TokArrow         -- "->"
  | TokPeriod        -- "."
  | TokComma         -- ","
  | TokGo            -- "go"
  | TokTake          -- "take"
  | TokMark          -- "mark"
  | TokNothing       -- "nothing"
  | TokTurn          -- "turn"
  | TokCase          -- "case"
  | TokOf            -- "of"
  | TokEnd           -- "end"
  | TokLeft          -- "left"
  | TokRight         -- "right"
  | TokFront         -- "front"
  | TokSemicolon     -- ";"
  | TokEmpty         -- "Empty"
  | TokLambda        -- "Lambda"
  | TokDebris        -- "Debris"
  | TokAsteroid      -- "Asteroid"
  | TokBoundary      -- "Boundary"
  | TokWildcard      -- "_"
  | TokIdent String  -- Identifier

-- Exercise 2
data Program = Program deriving Show
    | Command Command


data Command = Command deriving Show
    | Turn Left
    | Turn Right
