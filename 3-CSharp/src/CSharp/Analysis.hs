module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra

data AnalysisResult = Valid | Error ErrorType deriving Show
data ErrorType = ScopeError | TypeError deriving Show

type Vars = [(Ident,RetType)]
type Env = (Vars,Vars) -- Will contain a list of global variables and variables inside the method
data ValueOrAddress = Value IntOrBool | Address Ident | Assignment -- Three different possiblities of expressions that are utilized below
data IntOrBool = Int | Bool -- The two different possible types

analysisAlgebra :: CSharpAlgebra AnalysisResult (Env -> (Env,AnalysisResult)) (Env -> (Env,AnalysisResult)) (Env -> (Env,AnalysisResult,ValueOrAddress))
analysisAlgebra = CSharpAlgebra
  (\_ ms     -> snd $ foldl applyFunction (([],[]), Valid) ms) -- Class
  (\decl env -> ((\(global,local) -> (createVariable global decl,local)) env,Valid)) -- Method
  (\_ _ decls s (global,_) -> s (global, foldl createVariable [] decls)) -- Global variables
  (\decl (global,local) -> ((global,createVariable local decl),Valid)) -- Declarations
  (\e env -> (\(newEnv,b,_) -> (newEnv,b)) (e env)) -- Expressions
  (\e s1 s2 env -> (env,(\(_,b,_) -> b) (e env) `andAnalysis` snd (s1 env) `andAnalysis` snd (s2 env))) -- If statements
  (\e s1 env     ->(env,(\(_,b,_) -> b) (e env) `andAnalysis` snd (s1 env))) -- While loops
  (\e env       -> (\((global,_),b,_) -> ((global,[]),b)) (e env)) -- Return
  (\ss env      -> foldl applyFunction (env, Valid) ss) -- Statement blocks
  (\l env       -> (env,Valid,case l of  -- Literal value
    (LitInt _) -> Value Int
    (LitBool _) -> Value Bool)
  )
  (\v env       -> (env,,Address v)) -- Variable
  (\o e1 e2 env   -> if o == OpAsg
    then (env,checkAssignment ((\(_,_,v1) -> v1) $ e1 env) ((\(_,_,v2) -> v2) $ e2 env) env,Assignment) -- Assignment
    else (env,(\(_,b,_) -> b) (e1 env) `andAnalysis` (\(_,b,_) -> b) (e2 env),opType o)) -- Other operators
  (\ident es env     -> (\(newEnv,b) -> (newEnv,b,Address ident)) (foldl applyFunction2 (env, Valid) es)) -- Method calls
  where
    -- And function applied to analysis results
    andAnalysis :: AnalysisResult -> AnalysisResult -> AnalysisResult
    andAnalysis Valid Valid = Valid
    andAnalysis Valid e@(Error _) = e
    andAnalysis e@(Error _) _ = e

    -- Two specific methods for applying the functions made above in a fold like way
    applyFunction :: (Env, AnalysisResult) -> (Env -> (Env,AnalysisResult)) -> (Env, AnalysisResult)
    applyFunction (env, b1) func =
      let (newEnv, b2) = func env
      in (newEnv, b1 `andAnalysis` b2)
    applyFunction2 :: (Env, AnalysisResult) -> (Env -> (Env,AnalysisResult,ValueOrAddress)) -> (Env, AnalysisResult)
    applyFunction2 (env, b1) func =
      let (newEnv, b2, _) = func env
      in (newEnv, b1 `andAnalysis` b2)

    -- Creates the variable in the list
    createVariable :: Vars -> Decl -> Vars
    createVariable env (Decl retType ident) = env ++ [(ident,retType)]

    -- Checks if the assignment contains any errors
    checkAssignment :: ValueOrAddress -> ValueOrAddress -> Env -> AnalysisResult
    checkAssignment (Address var) (Value value) (global,local) =
      case lookup var local of
        Just rT -> case rT of
          TyVoid -> Valid
          NV t   -> compareTypes t value
        Nothing -> case lookup var global of
          Just rT -> case rT of
            TyVoid -> Valid
            NV t   -> compareTypes t value
          Nothing -> Error ScopeError
    checkAssignment _ _ _ = Error TypeError

    -- Checks for type errors
    compareTypes :: Type -> IntOrBool -> AnalysisResult
    compareTypes TyBool Bool = Valid
    compareTypes TyInt Int   = Valid
    compareTypes _ _         = Error TypeError

    -- Returns the type of the operators
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
