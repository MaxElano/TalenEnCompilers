{
module Parser where

import Model
}

%name parser
%tokentype { Token }
%error { parseError }

-- Token definitions
%token
  TokArrow        { TokArrow }
  TokPeriod       { TokPeriod }
  TokComma        { TokComma }
  TokSemicolon    { TokSemicolon }
  TokGo           { TokGo }
  TokTake         { TokTake }
  TokMark         { TokMark }
  TokNothing      { TokNothing }
  TokTurn         { TokTurn }
  TokCase         { TokCase }
  TokOf           { TokOf }
  TokEnd          { TokEnd }
  TokLeft         { TokLeft }
  TokRight        { TokRight }
  TokFront        { TokFront }
  TokEmpty        { TokEmpty }
  TokLambda       { TokLambda }
  TokDebris       { TokDebris }
  TokAsteroid     { TokAsteroid }
  TokBoundary     { TokBoundary }
  TokWildcard     { TokWildcard }
  TokIdent        { TokIdent $$ }

%%

-- Grammar rules
Program :: { [Rule] }
  : Rules                     { $1 }

Rules :: { [Rule] }
  : Rule                      { [$1] }
  | Rule Rules                { $1 : $2 }

Rule :: { Rule }
  : TokIdent TokArrow Cmds TokPeriod { Rule $1 $3 }

Cmds :: { [Command] }
  : {- empty -}                { [] }
  | Cmd                        { [$1] }
  | Cmd TokComma Cmds          { $1 : $3 }

Cmd :: { Command }
  : TokGo                      { ComGo }
  | TokTake                    { ComTake }
  | TokMark                    { ComMark }
  | TokNothing                 { ComNothing }
  | TokTurn Dir                { ComTurn $2 }
  | TokCase Dir TokOf Alts TokEnd { ComCase $2 $4 }
  | TokIdent                   { ComIdent $1 }

Dir :: { Direction }
  : TokLeft                    { DirLeft }
  | TokRight                   { DirRight }
  | TokFront                   { DirFront }

Alts :: { [Alt] }
  : {- empty -}                { [] }
  | Alt                        { [$1] }
  | Alt TokSemicolon Alts      { $1 : $3 }

Alt :: { Alt }
  : Pat TokArrow Cmds          { Alt $1 $3 }

Pat :: { Pattern }
  : TokEmpty                   { PatEmpty }
  | TokLambda                  { PatLambda }
  | TokDebris                  { PatDebris }
  | TokAsteroid                { PatAsteroid }
  | TokBoundary                { PatBoundary }
  | TokWildcard                { PatWildcard }


{
    parseError _ = error "Parse error"
}
