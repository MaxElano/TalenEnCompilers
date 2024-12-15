module Algebra where

import Model

import Data.Set (Set)
import qualified Data.Set as Set


-- Exercise 5
type Algebra r = 
    ( [r] -> r --Rules to program
    , Ident2 -> [r] -> r --Ident with commands to rule
    , Command -> r
    , Direction -> r 
    , Pattern -> r 
    , (Pattern, [r]) -> r
    )

fold :: Algebra r -> Program -> r
fold (proAlgebra, rulAlgebra, comAlgebra, dirAlgebra, patAlgebra, altAlgebra) = doProgram
  where
    setProgram :: Program -> r
    setProgram = proAlgebra . map setRule

    setRule :: Rule -> r
    setRule (Rule ident commands) = rulAlgebra ident (map setCommand commands)

    setCommand :: Command -> r 
    setCommand ComGo          = comAlgebra ComGo
    setCommand ComTake        = comAlgebra ComTake
    setCommand ComMark        = comAlgebra ComMark
    setCommand ComNothing     = comAlgebra ComNothing
    setCommand ComTurn d      = comAlgebra (ComTurn (dirAlgebra d))
    setCommand ComCase d alts = comAlgebra (ComCase (dirAlgebra d) (map setAlt alts))
    setCommand ComIdent text  = comAlgebra (ComIdent text)

    setAlt :: Alt -> r 
    setAlt (Alt pattern commands) = altAlgebra (Alt (patAlgebra pattern) (map setCommand commands))



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram program =
  let result = foldAnalysis analysisAlgebra program
  in  "start" `Set.member` definedRules result
      && Set.isSubsetOf (calledRules result) (definedRules result)
      && not (hasDuplicates result)
      && not (hasMissingCatchAll result)

data CheckResult = CheckResult
  { definedRules :: Set String  
  , calledRules  :: Set String  
  , hasDuplicates :: Bool       
  , hasMissingCatchAll :: Bool  
  }
  deriving (Show, Eq)

initialResult :: CheckResult
initialResult = CheckResult Set.empty Set.empty False False

type CheckAlgebra = 
  ( Set Ident2 -> CheckResult -> CheckResult 
  , Ident2 -> [CheckResult] -> CheckResult   
  , Command -> CheckResult                   
  , Direction -> CheckResult                 
  , Pattern -> Bool                          
  , (Bool, [CheckResult]) -> CheckResult     
  )

foldAnalysis :: CheckAlgebra -> Program -> CheckResult
foldAnalysis (combineRules, checkRule, checkCommand, _, checkPattern, checkAlt) program =
  foldr combineRules initialResult (map checkRule' program)
  where
    checkRule' (Rule name commands) =
      let ruleResult = checkRule name (map checkCommand commands)
      in ruleResult { definedRules = Set.insert name (definedRules ruleResult) }

    checkCommand ComGo          = initialResult
    checkCommand ComTake        = initialResult
    checkCommand ComMark        = initialResult
    checkCommand ComNothing     = initialResult
    checkCommand ComTurn _      = initialResult
    checkCommand ComCase _ alts = foldr (combineCheck checkAlt) initialResult (map checkAlt' alts)
    checkCommand ComIdent name  = initialResult { calledRules = Set.singleton name }

    checkAlt' (Alt pat cmds) =
      let patternCovered = checkPattern pat
          commandResults = map checkCommand cmds
      in (patternCovered, commandResults)

    combineCheck f (patternCovered, results) acc =
      let altResult = f (patternCovered, results)
      in acc
          { hasMissingCatchAll = hasMissingCatchAll acc || hasMissingCatchAll altResult }

checkAlgebra :: CheckAlgebra
checkAlgebra =
  ( \ruleSet acc -> acc { definedRules = Set.union ruleSet (definedRules acc) }
  , \name results ->
      let duplicates = any (\r -> name `Set.member` definedRules r) results
      in CheckResult
           { definedRules = Set.empty
           , calledRules = foldr Set.union Set.empty (map calledRules results)
           , hasDuplicates = duplicates || any hasDuplicates results
           , hasMissingCatchAll = any hasMissingCatchAll results
           }
  , \command -> case command of
      ComIdent name -> initialResult { calledRules = Set.singleton name }
      ComCase _ alts -> foldr (\(patternCovered, _) acc -> acc || not patternCovered) False alts
      _ -> initialResult
  , \_ -> initialResult -- Noop for directions
  , \pattern -> case pattern of
      PatWildcard -> True
      _           -> False
  , \(patternCovered, results) ->
      let hasWildcard = patternCovered
          subResults = foldr (\r acc -> acc || hasMissingCatchAll r) False results
      in initialResult { hasMissingCatchAll = not hasWildcard && subResults }
  )
