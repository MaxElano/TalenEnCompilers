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
type S = Env -> Env -> (Code, Env)  -- Statement (Local variables - Parameters -> code, local variables)
type E = Env -> Env -> ValueOrAddress -> Code -- Expression (Local variables - Parameters -> code)
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
  fExprMeth

fClass :: ClassName -> [M] -> C
fClass c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> M
fMembDecl d = []

fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps s = [LABEL x] ++ iniLocS ++ fst finCodEnv ++ iniLocE ++ [RET]
    where
        finCodEnv = s [] parToEnv
        iniLocS = [LDR MP] ++ [LDRR MP SP] ++ [AJS $ length (snd finCodEnv)]
        iniLocE = [LDRR SP MP] ++ [STR MP] ++ [STS $ -(length ps)] ++ [AJS $ -((length ps) - 1)]
        parToEnv = zipWith (\(Decl _ id) index -> (id, index)) ps (iterate (\l -> l - 1) (length ps + 1))

-- Fixed parameters until here (except for code in fMembMeth)

fStatDecl :: Decl -> S
fStatDecl (Decl _ i) = (\env par -> ([], [(i, nn env)] ++ env))
    where
        nn []  = 1
        nn env = (snd (head env)) + 1

fStatExpr :: E -> S
fStatExpr e = (\env par -> (e env par Value ++ [pop], env))

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 = (\env par -> (c env par ++ [BRF $ n1 env par + 2] ++ fst (s1 env par) ++ [BRA $ n2 env par] ++ fst (s2 (fEnv env par) par), sEnv env par)) where
  c env par  = e env par Value
  n1 env par = codeSize (fst (s1 env par))
  n2 env par = codeSize (fst (s2 env par))
  fEnv env par = snd (s1 env par)
  sEnv env par = snd (s2 (fEnv env par) par)

fStatWhile :: E -> S -> S
fStatWhile e s1 = (\env par -> ([BRA $ n env par] ++ fst (s1 env par) ++ c (fEnv env par) par ++ [BRT $ -(n (fEnv env par) par + k (fEnv env par) par + 2)], fEnv env par)) where
  c env par = e env par Value
  n env par = codeSize (fst (s1 env par))
  k env par = codeSize (c env par)
  fEnv env par= snd (s1 env par)

fStatReturn :: E -> S
fStatReturn e = (\env par -> (e env par Value ++ [STR R3] ++ [RET], env))

fStatBlock :: [S] -> S
fStatBlock ss = (\envOrg par -> foldl (\(code, env) s -> (code ++ fst (s env par), snd (s env par))) ([], envOrg) ss)

fExprLit :: Literal -> E
fExprLit l = (\env par va -> [LDC n]) where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

fExprVar :: Ident -> E
fExprVar x = (\env par va -> case va of
    Value   ->  [LDL  $ loc env par]
    Address ->  [LDLA $ loc env par])
  --where loc = 42
  where loc env par = case lookup x par of
          Just loc -> -loc
          Nothing -> (case lookup x env of
                    Just loc -> loc
                    Nothing  -> error "Variable not found")

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 = (\env par va -> e2 env par Value ++ [LDS 0] ++ e1 env par Address ++ [STA 0])
fExprOp OpAnd e1 e2 = (\env par va -> e1 env par Value ++ [BRT 4] ++ [STS $ bool2int True] ++ [BRA $ codeSize (e2 env par Value) + 2] ++ e2 env par Value ++ [AND])
fExprOp OpOr  e1 e2 = (\env par va -> e1 env par Value ++ [BRF 4] ++ [STS $ bool2int False] ++ [BRA $ codeSize (e2 env par Value) + 2] ++ e2 env par Value ++ [OR])
fExprOp op    e1 e2 = (\env par va -> e1 env par Value ++ e2 env par Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ])

fExprMeth :: Ident -> [E] -> E
fExprMeth "print" ps = parOnStack
    where
        parOnStack :: E
        parOnStack env par va = concatMap (\p -> p env par Value ++ [TRAP 0]) ps
fExprMeth id ps = \env par va -> parOnStack env par va ++ [Bsr id] ++ [LDR R3]
    where
        parOnStack :: E
        parOnStack env par va = concatMap (\p -> p env par Value) ps

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0
