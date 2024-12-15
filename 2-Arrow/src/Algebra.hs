module Algebra where

import Model


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
    setCommand ComGo                      = comAlgebra ComGo
    setCommand ComTake                    = comAlgebra ComTake
    setCommand ComMark                    = comAlgebra ComMark
    setCommand ComNothing                 = comAlgebra ComNothing
    setCommand ComTurn (Direction d)      = comAlgebra (ComTurn (dirAlgebra d))
    setCommand ComCase (Direction d) alts = comAlgebra (ComCase (dirAlgebra d) (map setAlt alts))
    setCommand ComIdent text              = comAlgebra (ComIdent text)

    setAlt :: Alt -> r 
    setAlt (Alt pattern commands) = altAlgebra (Alt (patAlgebra pattern) (map setCommand commands))


-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined