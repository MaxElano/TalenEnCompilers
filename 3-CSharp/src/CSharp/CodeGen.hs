module CSharp.CodeGen where

import CSharp.AbstractSyntax
import CSharp.Algebra

import SSM

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

{-
  This file contains a starting point for the code generation.
-}

-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type C = Code                   -- Class
type M = Code                   -- Member
type S = Code                   -- Statement
type E = ValueOrAddress -> Code -- Expression


codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
  fClass
  fMembDecl
  fMembMeth
  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock
  fExprLit
  fExprVar
  fExprOp

fClass :: ClassName -> [M] -> C
fClass c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> M
fMembDecl d = []

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps s = [LABEL x] ++ s ++ [RET]

fStatDecl :: Decl -> S
fStatDecl d = []

fStatExpr :: E -> S
fStatExpr e = e Value ++ [pop]

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2 where
  c        = e Value
  (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: E -> S -> S
fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))] where
  c = e Value
  (n, k) = (codeSize s1, codeSize c)

fStatReturn :: E -> S
fStatReturn e = e Value ++ [pop] ++ [RET]

fStatBlock :: [S] -> S
fStatBlock = concat

fExprLit :: Literal -> E
fExprLit l va  = [LDC n] where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x va = case va of
    Value   ->  [LDL  loc]
    Address ->  [LDLA loc]
  where loc = 42

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp op    e1 e2 va = e1 Value ++ e2 Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpAnd -> AND; OpOr -> OR; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0 
