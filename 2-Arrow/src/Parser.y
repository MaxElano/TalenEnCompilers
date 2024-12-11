{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  x { Token }

%%

Program : { Program }

{

happyError _ = error "parse error"

}