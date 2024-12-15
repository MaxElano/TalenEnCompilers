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
  | TokIdent String  
  | TokComment String
    deriving Show

-- Exercise 2
data Program = Program [Rule]
    deriving Show

data Rule = Rule Ident [Command]
    deriving Show

data Ident = Ident String 
    deriving Show

data Command = Command
    | ComGo
    | ComTake
    | ComMark
    | ComNothing
    | ComTurn Direction
    | ComCase Direction [Alt]
    | ComIdent
    deriving Show

data Direction = DirLeft
    | DirRight
    | DirFront
    deriving Show

data Alt = Alt Pattern [Command]
    deriving Show

data Pattern = PatEmpty 
    | PatLambda 
    | PatDebris 
    | PatAsteroid 
    | PatBoundary 
    | PatWildcard
    deriving show

