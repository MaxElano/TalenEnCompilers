{-# LANGUAGE RecordWildCards #-}
module CSharp.Algebra where

import CSharp.AbstractSyntax

{-
  Only modify this file when you change the AST in CSharpGram.hs
-}

data CSharpAlgebra c m s e
  = CSharpAlgebra
    { clas :: ClassName -> [m] -> c

    , memberD :: Decl -> m
    , memberM :: RetType -> String -> [Decl] -> s -> m

    , statDecl   :: Decl -> s
    , statExpr   :: e -> s
    , statIf     :: e -> s -> s  -> s
    , statWhile  :: e -> s -> s
    , statReturn :: e -> s
    , statBlock  :: [s] -> s

    , exprLit   :: Literal -> e
    , exprVar   :: Ident -> e
    , exprOper  :: Operator -> e -> e -> e
    , exprMeth  :: Ident -> [e] -> e
    }

-- The "{..}" notation brings all fields of the algebra into scope.
-- This means that, for example, 'memberD' in the 'fMemb' definition
-- refers to the 'memberD' field of the given algebra.
foldCSharp :: CSharpAlgebra c m s e -> Class -> c
foldCSharp CSharpAlgebra{..} = fClas where
  fClas (Class      t ms)     = clas t (map fMemb ms)
  fMemb (MemberD    d)        = memberD d
  fMemb (MemberM    t m ps s) = memberM t m ps (fStat s)
  fStat (StatDecl   d)        = statDecl d
  fStat (StatExpr   e)        = statExpr (fExpr e)
  fStat (StatIf     e s1 s2)  = statIf (fExpr e) (fStat s1) (fStat s2)
  fStat (StatWhile  e s1)     = statWhile (fExpr e) (fStat s1)
  fStat (StatReturn e)        = statReturn (fExpr e)
  fStat (StatBlock  ss)       = statBlock (map fStat ss)
  fExpr (ExprLit    lit)      = exprLit lit
  fExpr (ExprVar    var)      = exprVar var
  fExpr (ExprOper   op e1 e2) = exprOper op (fExpr e1) (fExpr e2)
  fExpr (ExprMeth   id ps)    = exprMeth id (map fExpr ps)