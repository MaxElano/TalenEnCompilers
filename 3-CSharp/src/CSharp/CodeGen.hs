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
type C = Env -> (Code, Env)                   -- Class
type M = Env -> (Code, Env)                   -- Member
type S = Env -> (Code, Env)                   -- Statement
type E = Env -> ValueOrAddress -> Code -- Expression
type Env = [(Decl, Ident)]

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
fClass c ms = (\envOrg -> ([Bsr "main", HALT] ++ fst (result envOrg), snd (result envOrg)))
    where result envOrg = foldl (\(code, env) s -> (code ++ fst (s env), snd (s env))) envOrg ms

fMembDecl :: Decl -> M
fMembDecl d = (\env -> ([], [(d, (snd head env) + 1)] ++ env))

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps s = (\env -> ([LABEL x] ++ fst (s env) ++ [RET] ++ [LDR MP] ++ [LDRR MP SP] ++ [AJS + length newEnv env] ++ createSSMCode newEnv env, newEnv env))
    where
        newEnv env = s env
        createSSMCode env = map (\(decl, index) -> [LDL index]) env

fStatDecl :: Decl -> S
fStatDecl d = (\env -> ([], [(d, (snd head env) + 1)] ++ env))

fStatExpr :: E -> S
fStatExpr e = (\env -> (e Value ++ [pop]))

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 = (\env -> (c ++ [BRF (n1 env + 2)] ++ fst b2 env ++ [BRA b3 env] ++ fst b4 env, snd (b4 env))) where
  c env       = e env Value
  n1 env = codeSize fst (s1 env)
  n2 env = codeSize fst (s2 env)
  b2 env = s1 (snd s1 env)
  b3 env = n2 (snd b2 env)
  b4 env = s2 (snd b2 env)

fStatWhile :: E -> S -> S
fStatWhile e s1 = (\env -> ([BRA fst n env] ++ fst s1 env ++ fst newEnv env ++ [BRT (-(fst n env + fst k env + 2))], snd newEnv env)) where
  c env = e env Value
  (n, k) = ((\env -> codeSize (s1 env)), (\env -> codeSize (c env)))
  newEnv env = c env

fStatReturn :: E -> S
fStatReturn e = (\env -> (e Value ++ [pop] ++ [RET], env))

fStatBlock :: [S] -> S
fStatBlock ss = (\envOrg -> foldl (\(code, env) s -> (code ++ fst (s env), snd (s env))) envOrg ss)

fExprLit :: Literal -> E
fExprLit l va = (\env -> [LDC n]) where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x va = (\env -> case va of
    Value   ->  [LDL  loc env]
    Address ->  [LDLA loc env])
  --where loc = 42
  where loc env = case M.lookup x env of
          Just loc -> loc
          Nothing  -> error "Variable not found"

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 va = (\env -> (fst (result2 env Value) ++ [LDS 0] ++ (result1 (snd (result2 env Value)) Address) ++ [STA 0], env))
fExprOp op    e1 e2 va = (\env -> (fst (result1 env Value) ++ fst (result2 (snd (result1 env Value)) Value) ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV
    ; OpMod -> MOD
    ; OpAnd -> AND; OpOr -> OR; OpXor -> XOR
    ; OpLeq -> LE; OpLt -> LT
    ; OpGeq -> GT; OpGt -> GT
    ; OpEq  -> EQ; OpNeq -> NE;}
  ]))
  where
    result1 env d = e1 env d
    result2 env d = e2 env d

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0 
