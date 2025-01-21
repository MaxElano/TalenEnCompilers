{-# LANGUAGE RecordWildCards #-}
module CSharp.Algebra where

import CSharp.AbstractSyntax

{-
  Only modify this file when you change the AST in CSharpGram.hs
-}

data CSharpAlgebra c m s e env
  = CSharpAlgebra
    { clas :: ClassName -> [m] -> env -> (c, env)

    , memberD :: Decl -> env -> (m, env)
    , memberM :: RetType -> String -> [Decl] -> s -> env -> (m, env)

    , statDecl   :: Decl -> env -> (s, env)
    , statExpr   :: e -> env -> (s, env)
    , statIf     :: e -> s -> s  -> env -> (s,env)
    , statWhile  :: e -> s -> env -> (s, env)
    , statReturn :: e -> env -> (s, env)
    , statBlock  :: [s] -> env -> (s, env)

    , exprLit   :: Literal -> env -> (e, env)
    , exprVar   :: Ident -> env -> (e, env)
    , exprOper  :: Operator -> e -> e -> env -> (e, env)
    }

-- The "{..}" notation brings all fields of the algebra into scope.
-- This means that, for example, 'memberD' in the 'fMemb' definition
-- refers to the 'memberD' field of the given algebra.
foldCSharp :: CSharpAlgebra c m s e env -> Class -> (c, env)
foldCSharp CSharpAlgebra{..} = fClas where
  fClas (Class      t ms)     env = (clas t (map fMemb ms), env)
  fMemb (MemberD    d)        env = (memberD d, env ++ [d])
  fMemb (MemberM    t m ps s) env = (memberM t m ps (fStat s), env)
  fStat (StatDecl   d)        env = (statDecl d, env ++ [d])
  fStat (StatExpr   e)        env = (statExpr (fExpr e), env)
  fStat (StatIf     e s1 s2)  env = (statIf (fExpr e) (fStat s1) (fStat s2), env)
  fStat (StatWhile  e s1)     env = (statWhile (fExpr e) (fStat s1), env)
  fStat (StatReturn e)        env = (statReturn (fExpr e), env)
  fStat (StatBlock  ss)       env = (statBlock (map fStat ss), env)
  fExpr (ExprLit    lit)      env = (exprLit lit, env)
  fExpr (ExprVar    var)      env = (exprVar var, env)
  fExpr (ExprOper   op e1 e2) env = (exprOper op (fExpr e1) (fExpr e2), env)
