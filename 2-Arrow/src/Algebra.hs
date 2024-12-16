{-# LANGUAGE TupleSections #-}

module Algebra where



import Model
import Data.List (nub, (\\))


-- Exercise 5

type Algebra r =
  ( [r] -> r                -- programAlg: Combine rules into a program
  , Ident2 -> [r] -> r       -- ruleAlg: Combine an identifier and commands into a rule
  , r                       -- cmdGoAlg: Represents the "go" command
  , r                       -- cmdTakeAlg: Represents the "take" command
  , r                       -- cmdMarkAlg: Represents the "mark" command
  , r                       -- cmdNothingAlg: Represents the "nothing" command
  , Direction -> r          -- cmdTurnAlg: Represents the "turn" command
  , Direction -> [r] -> r   -- cmdCaseAlg: Represents the "case" command
  , Ident2 -> r              -- cmdIdentAlg: Represents an identifier invocation
  , r                       -- dirLeftAlg: Represents the "left" direction
  , r                       -- dirRightAlg: Represents the "right" direction
  , r                       -- dirFrontAlg: Represents the "front" direction
  , Pattern -> [r] -> r     -- altAlg: Represents a pattern and commands
  , r                       -- patEmptyAlg: Represents the "Empty" pattern
  , r                       -- patLambdaAlg: Represents the "Lambda" pattern
  , r                       -- patDebrisAlg: Represents the "Debris" pattern
  , r                       -- patAsteroidAlg: Represents the "Asteroid" pattern
  , r                       -- patBoundaryAlg: Represents the "Boundary" pattern
  , r                       -- patWildcardAlg: Represents the "_" wildcard pattern
  )


-- Fold function for the program
foldProgram :: Algebra r -> Program -> r
foldProgram (programAlg, ruleAlg, cmdGoAlg, cmdTakeAlg, cmdMarkAlg, cmdNothingAlg,
             cmdTurnAlg, cmdCaseAlg, cmdIdentAlg, dirLeftAlg, dirRightAlg, dirFrontAlg,
             altAlg, patEmptyAlg, patLambdaAlg, patDebrisAlg, patAsteroidAlg,
             patBoundaryAlg, patWildcardAlg) = goProgram
  where
    goProgram :: Program -> r
    goProgram = programAlg . map goRule

    goRule :: Rule -> r
    goRule (Rule ident cmds) = ruleAlg ident (map goCmd cmds)

    goCmd :: Command -> r
    goCmd cmd = case cmd of
      ComGo              -> cmdGoAlg
      ComTake            -> cmdTakeAlg
      ComMark            -> cmdMarkAlg
      ComNothing         -> cmdNothingAlg
      ComTurn dir        -> cmdTurnAlg (goDir dir)
      ComCase dir alts   -> cmdCaseAlg (goDir dir) (map goAlt alts)
      ComIdent ident    -> cmdIdentAlg ident

    goDir :: Direction -> r
    goDir dir = case dir of
      DirLeft  -> dirLeftAlg
      DirRight -> dirRightAlg
      DirFront -> dirFrontAlg

    goAlt :: Alt -> r
    goAlt (Alt pat cmds) = altAlg (goPat pat) (map goCmd cmds)

    goPat :: Pattern -> r
    goPat pat = case pat of
      PatEmpty     -> patEmptyAlg
      PatLambda    -> patLambdaAlg
      PatDebris    -> patDebrisAlg
      PatAsteroid  -> patAsteroidAlg
      PatBoundary  -> patBoundaryAlg
      PatWildcard  -> patWildcardAlg

-- Exercise 6
checkProgram :: Program -> Bool
checkProgram program =
  let (definedRules, usedRules) = foldProgram collectRulesAlgebra program
      hasStart = foldProgram hasStartRuleAlgebra program
      allRules = foldProgram noDuplicateRulesAlgebra program
      exhaustive = foldProgram allCasesExhaustiveAlgebra program
   in and
        [ null (usedRules \\ definedRules) -- No undefined rules
        , hasStart -- "start" rule exists
        , length allRules == length (nub allRules) -- No duplicate rules
        , exhaustive -- All cases exhaustive
        ]

collectRulesAlgebra :: Algebra ([Ident2], [Ident2])
collectRulesAlgebra =
  ( unzip . map (\(defs, uses) -> (nub defs, nub uses)) -- Combine rules
  , \ident cmds -> (ident : concatMap fst cmds, concatMap snd cmds) -- Rule
  , ([], []) -- Go
  , ([], []) -- Take
  , ([], []) -- Mark
  , ([], []) -- NothingCmd
  , const ([], []) -- Turn
  , \_ cmds -> ([], concatMap snd cmds) -- Case
  , \ident -> ([], [ident]) -- Invoke
  , ([], []) -- LeftDir
  , ([], []) -- RightDir
  , ([], []) -- FrontDir
  , \_ cmds -> ([], concatMap snd cmds) -- Alt
  , ([], []) -- Empty
  , ([], []) -- Lambda
  , ([], []) -- Debris
  , ([], []) -- Asteroid
  , ([], []) -- Boundary
  , ([], []) -- Wildcard
  )

hasStartRuleAlgebra :: Algebra Bool
hasStartRuleAlgebra =
  (or, -- Combine rules
   \ident _ -> ident == "start", -- Rule
   False, -- Go
   False, -- Take
   False, -- Mark
   False, -- NothingCmd
   const False, -- Turn
   const (const False), -- Case
   const False, -- Invoke
   False, -- LeftDir
   False, -- RightDir
   False, -- FrontDir
   const (const False), -- Alt
   False, -- Empty
   False, -- Lambda
   False, -- Debris
   False, -- Asteroid
   False, -- Boundary
   False -- Wildcard
  )

noDuplicateRulesAlgebra :: Algebra [Ident2]
noDuplicateRulesAlgebra =
  (concat, -- Combine rules
   \ident _ -> [ident], -- Rule
   [], -- Go
   [], -- Take
   [], -- Mark
   [], -- NothingCmd
   const [], -- Turn
   const (const []), -- Case
   const [], -- Invoke
   [], -- LeftDir
   [], -- RightDir
   [], -- FrontDir
   const (const []), -- Alt
   [], -- Empty
   [], -- Lambda
   [], -- Debris
   [], -- Asteroid
   [], -- Boundary
   [] -- Wildcard
  )

allCasesExhaustiveAlgebra :: Algebra Bool
allCasesExhaustiveAlgebra =
  (and -- Combine rules
  , const and, -- Rule
   True, -- Go
   True, -- Take
   True, -- Mark
   True, -- NothingCmd
   const True, -- Turn
   \_ alts -> any (== PatWildcard) (map fst alts) || -- Case: catch-all
               all (`elem` map fst alts) [PatEmpty, PatLambda, PatDebris, PatAsteroid, PatBoundary], -- All patterns
   const True, -- Invoke
   True, -- LeftDir
   True, -- RightDir
   True, -- FrontDir
   const (const True), -- Alt
   True, -- Empty
   True, -- Lambda
   True, -- Debris
   True, -- Asteroid
   True, -- Boundary
  )
