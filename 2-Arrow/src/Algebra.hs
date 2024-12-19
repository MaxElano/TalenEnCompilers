module Algebra where
import Model
import Data.List (nub)
import qualified Data.Set as S


-- Exercise 5
type Algebra pr r c a =
  ( [r] -> pr             -- programAlg
  , Ident2 -> [c] -> r    -- ruleAlg
  , Command -> c          -- "nothing", "mark", "take, "go" command
  , Direction -> c        -- cmdTurnAlg
  , Direction -> [a] -> c -- cmdCaseAlg
  , Ident2 -> c           -- cmdIdentAlg
  , Pattern -> [c] -> a   -- altAlg
  )

-- Fold function for the program
foldProgram :: Algebra pr r c a -> Program -> pr
foldProgram (programAlg, ruleAlg, cmdGenAlg, cmdTurnAlg, cmdCaseAlg,
             cmdIdentAlg, altAlg) = goProgram
  where
    --goProgram :: Program -> pr
    goProgram = programAlg . map goRule

    --goRule :: Rule -> r
    goRule (Rule ident cmds) = ruleAlg ident (map goCmd cmds)

    --goCmd :: Command -> c
    goCmd cmd = case cmd of
      ComGo              -> cmdGenAlg ComGo
      ComTake            -> cmdGenAlg ComTake
      ComMark            -> cmdGenAlg ComMark
      ComNothing         -> cmdGenAlg ComNothing
      ComTurn dir        -> cmdTurnAlg dir
      ComCase dir alts   -> cmdCaseAlg dir (map goAlt alts)
      ComIdent ident     -> cmdIdentAlg ident

    --goAlt :: Alt -> a
    goAlt (Alt pat cmds) = altAlg pat (map goCmd cmds)


-- Exercise 6
checkProgram :: Program -> Bool
checkProgram program =
  let (definedRules, usedRules) = foldProgram collectRulesAlgebra program
      hasStart = foldProgram hasStartRuleAlgebra program
      allRules = foldProgram noDuplicateRulesAlgebra program
      exhaustive = foldProgram allCasesExhaustiveAlgebra program
   in and
        [ not (any (`S.notMember` S.fromList definedRules) usedRules) -- No undefined rules
        , hasStart -- "start" rule exists
        , length allRules == length (nub allRules) -- No duplicate rules
        , exhaustive -- All cases exhaustive
        ]

collectRulesAlgebra :: Algebra ([Ident2], [Ident2]) ([Ident2], [Ident2]) ([Ident2], [Ident2]) ([Ident2], [Ident2])
collectRulesAlgebra =
 ( foldl (\(defsnew, usesnew) (defsxs, usexs) -> (defsnew ++ defsxs, usesnew ++ usexs)) ([], []) . map (\(defs, uses) -> (nub defs, nub uses)) -- Combine rules
 , \ident cmds -> (ident : concatMap fst cmds, concatMap snd cmds) -- Rule
 , const ([], []) -- Go, Take, Mark, NothingCmd
 , const ([], []) -- Turn
 , \_ cmds -> ([], concatMap snd cmds) -- Case
 , \ident -> ([], [ident]) -- Invoke
 , \_ cmds -> ([], concatMap snd cmds) -- Alt
 )

hasStartRuleAlgebra :: Algebra Bool Bool Bool Bool
hasStartRuleAlgebra =
  (or, -- Combine rules
   \ident _ -> ident == "start", -- Rule
   const False, -- Go, Take, Mark, NothingCmd
   const False, -- Turn
   const (const False), -- Case
   const False, -- Invoke
   const (const False) -- Alt
  )

noDuplicateRulesAlgebra :: Algebra [Ident2] [Ident2] [Ident2] [Ident2]
noDuplicateRulesAlgebra =
  (concat, -- Combine rules
   \ident _ -> [ident], -- Rule
   const [], -- Go, Take, Mark, NothingCmd
   const [], -- Turn
   const (const []), -- Case
   const [], -- Invoke
   const (const []) -- Alt
  )

allCasesExhaustiveAlgebra :: Algebra Bool Bool Bool Pattern
allCasesExhaustiveAlgebra =
 (and, -- Combine rules
  const and, -- Rule
  const True, -- Go, Take, Mark, NothingCmd
  const True, -- Turn
  \_ pats -> elem PatWildcard pats || --Wildcard in use
              all (`elem` pats) [PatEmpty, PatLambda, PatDebris, PatAsteroid, PatBoundary], -- All patterns
  const True, -- Invoke
  const -- Alt
 )
