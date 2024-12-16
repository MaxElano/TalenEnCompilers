module Algebra where

import Model
import Data.List (nub, (\\))


-- Exercise 5
data ProgramAlg r = ProgramAlg
  { algRule :: Ident2 -> [r] -> r -- Handles a rule
  , algCommand :: Command -> r    -- Handles a command
  , algDirection :: Direction -> r -- Handles a direction
  , algAlt :: Pattern -> [r] -> r -- Handles an alternative
  , algPattern :: Pattern -> r    -- Handles a pattern
  }

-- Fold function for the program
foldProgram :: ProgramAlg r -> Program -> [r]
foldProgram alg = map (foldRule alg)

foldRule :: ProgramAlg r -> Rule -> r
foldRule alg (Rule ident cmds) = algRule alg ident (map (foldCommand alg) cmds)

foldCommand :: ProgramAlg r -> Command -> r
foldCommand alg cmd = case cmd of
  ComGo            -> algCommand alg ComGo
  ComTake          -> algCommand alg ComTake
  ComMark          -> algCommand alg ComMark
  ComNothing       -> algCommand alg ComNothing
  ComTurn dir      -> algCommand alg (ComTurn (foldDirection alg dir))
  ComCase dir alts -> algCommand alg (ComCase (foldDirection alg dir) (map (foldAlt alg) alts))
  ComIdent ident   -> algCommand alg (ComIdent ident)

foldDirection :: ProgramAlg r -> Direction -> r
foldDirection alg dir = algDirection alg dir

foldAlt :: ProgramAlg r -> Alt -> r
foldAlt alg (Alt pat cmds) = algAlt alg (foldPattern alg pat) (map (foldCommand alg) cmds)

foldPattern :: ProgramAlg r -> Pattern -> r
foldPattern alg pat = algPattern alg pat



-- Exercise 6
checkProgram :: Program -> Bool
checkProgram program = and
  [ noUndefinedRules program
  , hasStartRule program
  , noDuplicateRules program
  , allCasesExhaustive program
  ]

-- 1. Check for undefined rules
noUndefinedRules :: Program -> Bool
noUndefinedRules program =
  let definedRules = map (\(Rule ident _) -> ident) program
      usedRules = collectUsedRules program
   in null (usedRules \\ definedRules) -- All used rules must be defined

collectUsedRules :: Program -> [Ident2]
collectUsedRules = foldProgram usedRulesAlg

usedRulesAlg :: ProgramAlg [Ident2]
usedRulesAlg = ProgramAlg
  { algRule = \_ cmds -> concat cmds
  , algCommand = \cmd -> case cmd of
      ComIdent ident -> [ident]
      ComCase _ alts -> concat alts
      _              -> []
  , algDirection = const []
  , algAlt = \_ cmds -> concat cmds
  , algPattern = const []
  }

-- 2. Check if the program contains a "start" rule
hasStartRule :: Program -> Bool
hasStartRule program = "start" `elem` map (\(Rule ident _) -> ident) program

-- 3. Check for duplicate rule definitions
noDuplicateRules :: Program -> Bool
noDuplicateRules program =
  let ruleNames = map (\(Rule ident _) -> ident) program
   in length ruleNames == length (nub ruleNames)

-- 4. Check that all cases are exhaustive
allCasesExhaustive :: Program -> Bool
allCasesExhaustive = all isCaseExhaustive . collectCases

collectCases :: Program -> [Command]
collectCases = foldProgram caseCollectorAlg

caseCollectorAlg :: ProgramAlg [Command]
caseCollectorAlg = ProgramAlg
  { algRule = \_ cmds -> concat cmds
  , algCommand = \cmd -> case cmd of
      ComCase _ alts -> [ComCase undefined alts]
      _              -> []
  , algDirection = const []
  , algAlt = \_ cmds -> concat cmds
  , algPattern = const []
  }

isCaseExhaustive :: Command -> Bool
isCaseExhaustive (ComCase _ alts) =
  let patterns = map (\(Alt pat _) -> pat) alts
   in PatWildcard `elem` patterns || all (`elem` patterns) [PatEmpty, PatLambda, PatDebris, PatAsteroid, PatBoundary]
isCaseExhaustive _ = True
