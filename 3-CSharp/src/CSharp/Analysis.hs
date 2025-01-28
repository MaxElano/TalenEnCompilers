module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra
import qualified Data.Map as M

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
  undefined

type Types = M.Map Ident RetType
data ValueOrAddress = Value IntOrBool | Address Ident | Assignment
data IntOrBool = Int | Bool

typeCheckingAlgebra :: CSharpAlgebra Bool (Types -> (Types,Bool)) (Types -> (Types,Bool)) (Types -> (Types,Bool,ValueOrAddress))
typeCheckingAlgebra = CSharpAlgebra
  (\_ ms     -> snd $ foldl applyFunction (M.empty, True) ms)
  (\(Decl retType ident) types -> (M.insert ident retType types,True))
  (\_ _ _ s types -> s types)
  (\decl types -> (createVariable types decl,True))
  (\e types -> (\(newTypes,b,_) -> (newTypes,b)) (e types))
  (\e s1 s2 types -> (types,(\(_,b,_) -> b) (e types) && snd (s1 types) && snd (s2 types)))
  (\e s1 types     -> (types,(\(_,b,_) -> b) (e types) && snd (s1 types)))
  (\e types       -> (\(newTypes,b,_) -> (newTypes,b)) (e types))
  (\ss types      -> foldl applyFunction (types, True) ss)
  (\l types       -> (types,True,case l of 
    (LitInt _) -> Value Int
    (LitBool _) -> Value Bool
    )
  )
  (\v types       -> (types,True,Address v))
  (\o e1 e2 types   -> if o == OpAsg
    then (types,checkAssignment ((\(_,_,v1) -> v1) $ e1 types) ((\(_,_,v2) -> v2) $ e2 types) types,Assignment)
    else (types,(\(_,b,_) -> b) (e1 types) && (\(_,b,_) -> b) (e2 types),opType o))
  (\ident es types     -> (\(newTypes,b) -> (newTypes,b,Address ident)) (foldl applyFunction2 (types, True) es))
  where
    applyFunction :: (Types, Bool) -> (Types -> (Types,Bool)) -> (Types, Bool)
    applyFunction (types, b1) func =
      let (types, b2) = func types
      in (types, b1 && b2)
    applyFunction2 :: (Types, Bool) -> (Types -> (Types,Bool,ValueOrAddress)) -> (Types, Bool)
    applyFunction2 (types, b1) func =
      let (types, b2, _) = func types
      in (types, b1 && b2)

    createVariable :: Types -> Decl -> Types
    createVariable types (Decl retType ident) = M.insert ident retType types

    checkAssignment :: ValueOrAddress -> ValueOrAddress -> Types -> Bool
    checkAssignment (Address var) (Value value) types =
      let retType = case M.lookup var types of
            Just rT -> rT
            Nothing -> error "Variable does not exist"
      in case retType of
        TyVoid -> False
        NV t   -> compareTypes t value
    checkAssignment _ _ _ = False

    compareTypes :: Type -> IntOrBool -> Bool
    compareTypes TyBool Bool = True
    compareTypes TyInt Int   = True
    compareTypes _ _                = False

    opType :: Operator -> ValueOrAddress
    opType OpAdd = Value Int
    opType OpSub = Value Int
    opType OpMul = Value Int
    opType OpDiv = Value Int
    opType OpMod = Value Int
    opType OpAnd = Value Bool
    opType OpOr = Value Bool
    opType OpXor = Value Bool
    opType OpLeq = Value Bool
    opType OpLt = Value Bool
    opType OpGeq = Value Bool
    opType OpGt = Value Bool
    opType OpEq = Value Bool
    opType OpNeq = Value Bool
    opType OpAsg = Assignment
