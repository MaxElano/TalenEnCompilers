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
type C = Code             -- Class
type M = Code             -- Member
type S = Env -> (Code, Env)                   -- Statement
type E = Env -> ValueOrAddress -> Code -- Expression
type Env = [(Ident, Int)]

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

--fClass c ms = (\envOrg -> ([Bsr "main", HALT] ++ fst (result envOrg), snd (result envOrg)))
--    where result envOrg = foldl (\(code, env) s -> (code ++ fst (s env), snd (s env))) envOrg ms

fMembDecl :: Decl -> M
fMembDecl d = []
--fMembDecl d = (\env -> ([], [(d, (snd head env) + 1)] ++ env))

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps s = [LABEL x] ++ iniParS ++ fst finCodEnv ++ iniParE ++ [RET]
    where
        finCodEnv = s []
        iniParS = [LDR MP] ++ [LDRR MP SP] ++ [AJS (length (snd finCodEnv))] --iniVar --Maybe implement the store of the decleration, but that is silly
        iniParE = [LDRR SP MP] ++ [STR MP]
        --iniVar = map (iniV) (snd finCodEnv)
        --iniV = 

fStatDecl :: Decl -> S
fStatDecl (Decl _ i) = (\env -> ([], [(i, nn env)] ++ env))
    where
        nn []  = 1
        nn env = (snd (head env)) + 1

fStatExpr :: E -> S
fStatExpr e = (\env -> (e env Value ++ [pop], env))

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 = (\env -> (c env ++ [BRF (n1 env + 2)] ++ fst (s1 env) ++ [BRA (n2 env)] ++ fst (s2 (fEnv env)), sEnv env)) where
  c env  = e env Value
  n1 env = codeSize (fst (s1 env))
  n2 env = codeSize (fst (s2 env))
  fEnv env = snd (s1 env)
  sEnv env = snd (s2 (fEnv env))

fStatWhile :: E -> S -> S
fStatWhile e s1 = (\env -> ([BRA (n env)] ++ fst (s1 env) ++ c (fEnv env) ++ [BRT (-(n (fEnv env) + k (fEnv env) + 2))], fEnv env)) where
  c env = e env Value
  n env = codeSize (fst (s1 env))
  k env = codeSize (c env)
  fEnv env = snd (s1 env)

fStatReturn :: E -> S
fStatReturn e = (\env -> (e env Value ++ [pop] ++ [RET], env))

fStatBlock :: [S] -> S
fStatBlock ss = (\envOrg -> foldl (\(code, env) s -> (code ++ fst (s env), snd (s env))) ([], envOrg) ss)

fExprLit :: Literal -> E
fExprLit l va = (\env -> [LDC n]) where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x = (\env va -> case va of
    Value   ->  [LDL  (loc env)]
    Address ->  [LDLA (loc env)])
  --where loc = 42
  where loc env = case lookup x env of
          Just loc -> loc
          Nothing  -> error "Variable not found"

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 = (\env va -> e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0]) --This needs to take env in account
fExprOp OpAnd e1 e2 = (\env va -> e1 env Value ++ [BRT 4] ++ [STS $ bool2int True] ++ [BRA $ codeSize (e2 env Value) + 2] ++ e2 env Value ++ [AND])
fExprOp OpOr  e1 e2 = (\env va -> e1 env Value ++ [BRF 4] ++ [STS $ bool2int False] ++ [BRA $ codeSize (e2 env Value) + 2] ++ e2 env Value ++ [OR])
fExprOp op    e1 e2 = (\env va -> e1 env Value ++ e2 env Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ])

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0 
