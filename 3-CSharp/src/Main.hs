--Names:
--Max Ferket 7337248
--Daniël van der Hoeven 5041503
module Main where

import CSharp.Algebra
import CSharp.Analysis
import CSharp.CodeGen
import CSharp.AbstractSyntax
import CSharp.Parser

import SSM

import ParseLib.Derived

import System.Environment
import System.FilePath
import CSharp.Parser (lexicalScanner)
import CSharp.CodeGen (codeAlgebra)

main :: IO ()
main = do
  -- get command line arguments
  args <- getArgs
  files <- case args of
    [] -> do
      putStrLn "no argument given; assuming example.cs"
      return ["example.cs"]
    xs -> return xs
  -- translate each of the files
  mapM_ processFile files

-- -- processFile compiles one file;
-- -- it take the name of the input file
processFile :: FilePath -> IO ()
processFile infile = do
  let outfile = addExtension (dropExtension infile) "ssm"
  xs <- readFile infile
  let program = run "parser" (pClass <* eof) . run "lexer" lexicalScanner $ xs 
  case foldCSharp analysisAlgebra program of
    Valid -> do
      let ssm = formatCode $ foldCSharp codeAlgebra program
      writeFile outfile ssm
      putStrLn (outfile ++ " written")
    Error ScopeError -> error "Scope error detected"
    Error TypeError -> error "Type error detected"

run :: String -> Parser s a -> [s] -> a
run s p = fst . headOrError . parse (p <* eof)
    where
  headOrError (x : xs) = x
  headOrError [] = error $ "The " <> s <> " returned no full parses."
