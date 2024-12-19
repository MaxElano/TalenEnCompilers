{
module Lexer where

import Model
}

%wrapper "basic"

-- Define the tokens
tokens :-
  $white+       ;
  "->"          { \s -> TokArrow }
  "."           { \s -> TokPeriod }
  ","           { \s -> TokComma }
  ";"           { \s -> TokSemicolon }
  "_"           { \s -> TokWildcard }
  "--"[^\n]     { \s -> TokComment s}
  "go"          { \s -> TokGo }
  "take"        { \s -> TokTake }
  "mark"        { \s -> TokMark }
  "nothing"     { \s -> TokNothing }
  "turn"        { \s -> TokTurn }
  "case"        { \s -> TokCase }
  "of"          { \s -> TokOf }
  "end"         { \s -> TokEnd }
  "left"        { \s -> TokLeft }
  "right"       { \s -> TokRight }
  "front"       { \s -> TokFront }
  "Empty"       { \s -> TokEmpty }
  "Lambda"      { \s -> TokLambda }
  "Debris"      { \s -> TokDebris }
  "Asteroid"    { \s -> TokAsteroid }
  "Boundary"    { \s -> TokBoundary }
  [a-zA-Z0-9\+\-]+ { \s -> TokIdent s }
  .             { \s -> error $ "Invalid character: " ++ [head s] }
