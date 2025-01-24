module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra

-- data AnalysisResult 
--   = Valid
--   | CompilerError

data AnalysisResult = Valid -- add other constructors for any errors you add analyses for

analysisAlgebra :: CSharpAlgebra AnalysisResult () () ()
analysisAlgebra = undefinedAlgebra { clas = \_ _ -> Valid }

undefinedAlgebra :: CSharpAlgebra a b c d
undefinedAlgebra = CSharpAlgebra
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined
  undefined