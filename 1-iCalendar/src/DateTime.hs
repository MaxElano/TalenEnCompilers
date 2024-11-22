module DateTime where

import ParseLib

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord, Show)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* parseSep <*> parseTime <*> parseUtc

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = Year <$> quadParseInt

parseMonth :: Parser Char Month
parseMonth = Month <$> doubleParseInt

parseDay :: Parser Char Day
parseDay = Day <$> doubleParseInt

parseSep :: Parser Char Bool
parseSep = (\x -> x =='T') <$> symbol 'T'

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = Hour <$> doubleParseInt

parseMinute :: Parser Char Minute
parseMinute = Minute <$> doubleParseInt

parseSecond :: Parser Char Second
parseSecond = Second <$> doubleParseInt

quadParseInt :: Parser Char Int
quadParseInt = (\x y z w -> x * 1000 + y * 100 + z * 10 + w) <$> newdigit <*> newdigit <*> newdigit <*> newdigit 

doubleParseInt :: Parser Char Int
doubleParseInt = (\x y -> x * 10 + y) <$> newdigit <*> newdigit

--Created like this, so left side will be True if there is a Z and also take the Z away.
--But the right side will always be False and leave the symbols in the text.
parseUtc :: Parser Char Bool
parseUtc = (\x -> x =='Z') <$> symbol 'Z' 
    <<|> (\x -> x =='Z') <$> succeed 'T'

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run = undefined

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime date time utc) = printDate date ++ printSep ++ printTime time ++ printUtc utc

printDate :: Date -> String
printDate (Date year month day) = printYear year ++ printMonth month ++ printDay day

printYear :: Year -> String
printYear (Year runYear) = printAmountOfString runYear 4

printMonth :: Month -> String
printMonth (Month runMonth) = printAmountOfString runMonth 2

printDay :: Day -> String
printDay (Day runDay) = printAmountOfString runDay 2

printSep :: String
printSep = "T"

printTime :: Time -> String
printTime (Time hour minute second) = printHour hour ++ printMinute minute ++ printSecond second

printHour :: Hour -> String
printHour (Hour runHour) = printAmountOfString runHour 2

printMinute :: Minute -> String
printMinute (Minute runMinute) = printAmountOfString runMinute 2

printSecond :: Second -> String
printSecond (Second runSecond) = printAmountOfString runSecond 2

printUtc :: Bool -> String
printUtc True  = "Z"
printUtc False = ""

printAmountOfString :: Int -> Int -> String
printAmountOfString value index = addZeros (show value) index
    where addZeros :: String -> Int -> String
          addZeros xs index = (replicate (index - length xs) '0') ++ xs

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
