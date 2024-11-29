-- You can use this file to test your functions: `cabal run` executes main.
-- For example, if main is set to mainDateTime or mainCalendar:
-- echo "19970610T172345Z" | cabal run
-- cat examples/bastille.ics | cabal run
-- Feel free to use ghci instead, or to change functions here to test whatever you want.
-- We'll ignore anything in this file when grading!

--Names:
--Max Ferket 7337248
--Daniël van der Hoeven 5041503

module Main where

import DateTime
import Calendar
import Features
import System.Environment
import System.IO


data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main = do
  setNewlineTranslations
  mainCalendar

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
  where
    processInput = map (run parseDateTime) . lines
    processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
    printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = interact (show . recognizeCalendar)

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
  string <- readFileWindows path
  return $ recognizeCalendar string

-- These three functions fight Windows newline translations:
-- without them, on Windows machines, "\r\n" will be read as "\n"
-- and "\n" will be written as "\r\n".
-- Test using these functions rather than "readFile", and parse  newlines as "\r\n",
-- to make sure your parser works on all operating systems (i.e. also for your grader)!
setNewlineTranslations :: IO ()
setNewlineTranslations = do
  hSetNewlineMode stdin  noNewlineTranslation
  hSetNewlineMode stdout noNewlineTranslation
readFileWindows :: FilePath -> IO String
readFileWindows p = withBinaryFile p ReadMode hGetContents
writeFileWindows :: FilePath -> String -> IO ()
writeFileWindows p s = withBinaryFile p WriteMode (`hPutStr` s)
