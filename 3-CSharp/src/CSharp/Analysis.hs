module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra

data AnalysisResult 
  = Valid
  | CompilerError

analysisAlgebra :: CSharpAlgebra AnalysisResult () () ()
analysisAlgebra = undefinedAlgebra { clas = \_ _ -> Valid }

undefinedAlgebra :: CSharpAlgebra a b c d -> AnalysisResult
undefinedAlgebra = CSharpAlgebra
  fClas (Class      t ms)      = Valid
  fMemb (MemberD    d)         = Valid
  fMemb (MemberM    t m ps s)  = memberM t m ps (fStat s)
  fStat (StatDecl   d)         = statDecl d
  fStat (StatExpr   e)         = statExpr (fExpr e)
  fStat (StatIf     e s1 s2)   = statIf (fExpr e) (fStat s1) (fStat s2)
  fStat (StatWhile  e s1)      = statWhile (fExpr e) (fStat s1)
  fStat (StatReturn e)         = statReturn (fExpr e)
  fStat (StatBlock  ss)        = statBlock (map fStat ss)
  fExpr (ExprLit    lit)       = exprLit lit
  fExpr (ExprVar    var)       = exprVar var
  fExpr (ExprOper   op e1 e2)  = exprOper op (fExpr e1) (fExpr e2)
