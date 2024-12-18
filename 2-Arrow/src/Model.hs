module Model where

-- Exercise 1
data Token = Token
  | TokArrow         
  | TokPeriod        
  | TokComma         
  | TokGo            
  | TokTake          
  | TokMark          
  | TokNothing       
  | TokTurn          
  | TokCase          
  | TokOf            
  | TokEnd           
  | TokLeft          
  | TokRight         
  | TokFront         
  | TokSemicolon     
  | TokEmpty         
  | TokLambda        
  | TokDebris        
  | TokAsteroid      
  | TokBoundary      
  | TokWildcard      
  | TokIdent Ident2  
  | TokComment String
    deriving (Show, Eq)

-- Exercise 2
type Program = [Rule]

data Rule = Rule Ident2 [Command]
    deriving (Show, Eq)

type Ident2 = String 

data Command
    = ComGo
    | ComTake
    | ComMark
    | ComNothing
    | ComTurn Direction
    | ComCase Direction [Alt]
    | ComIdent Ident2
    deriving (Show, Eq)

data Direction 
    = DirLeft
    | DirRight
    | DirFront
    deriving (Show, Eq)

data Alt = Alt Pattern [Command]
    deriving (Show, Eq)

data Pattern = PatEmpty 
    | PatLambda 
    | PatDebris 
    | PatAsteroid 
    | PatBoundary 
    | PatWildcard
    deriving (Show, Eq)

