{
module Lexer where

import Model
}

%wrapper "basic"


--$whitespace = [ \t\n\r]+
$symbolic = '->' | \. | , | ; | _
$keywords = go | take | mark | nothing | turn | case | of | end | left | right | front | Empty | Lambda | Debris | Asteroid | Boundary
$ident = [a-zA-Z0-9\+\-]+
--$comment = "--"[^\n]*
&comment = "--".* 


tokens :-

$white+           ;
$comment          ;
$symbolic          { case $$ of
                        "->" -> TokArrow
                        "."  -> TokPeriod
                        ","  -> TokComma
                        ";"  -> TokSemicolon
                        "_"  -> TokWildcard
                    }
$keywords          { case $$ of
                        "go"        -> TokGo
                        "take"      -> TokTake
                        "mark"      -> TokMark
                        "nothing"   -> TokNothing
                        "turn"      -> TokTurn
                        "case"      -> TokCase
                        "of"        -> TokOf
                        "end"       -> TokEnd
                        "left"      -> TokLeft
                        "right"     -> TokRight
                        "front"     -> TokFront
                        "Empty"     -> TokEmpty
                        "Lambda"    -> TokLambda
                        "Debris"    -> TokDebris
                        "Asteroid"  -> TokAsteroid
                        "Boundary"  -> TokBoundary
                    }
$ident             { TokIdent $$ }

-- Catch-all for invalid characters
.                  { error $ "Invalid character: " ++ [head $$] }