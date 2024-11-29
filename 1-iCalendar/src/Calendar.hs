{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Calendar where

import ParseLib
import DateTime
import Data.Char (isAlpha)
import Data.List


-- Exercise 6
data Calendar = Calendar { calprop    :: [Calprop]
                         , eventList  :: [Event] }
    deriving (Eq, Ord, Show)

data Calprop
    = ProdID String
    | Version String
    deriving (Eq, Ord, Show)

data Event = Event [EventProp] deriving (Eq, Ord, Show)

data EventProp
    = DTStamp DateTime
    | UID String
    | DTStart DateTime
    | DTEnd DateTime
    | Description String
    | Summary String
    | Location String
    deriving (Eq,Ord,Show)

-- Exercise 7
newtype Token = Token { token :: String } deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = concatTokens . filterTokens <$> listOf (Token <$> many (satisfy (/= '\r'))) (ParseLib.token "\r\n")

filterTokens :: [Token] -> [Token]
filterTokens = filter (not . (\(Token t) -> null t))

concatTokens :: [Token] -> [Token]
concatTokens ((Token t1):(Token t2):ts)
    | head t2 == ' ' = Token (t1++drop 1 t2):concatTokens ts
    | otherwise      = Token t1 : concatTokens (Token t2:ts)
concatTokens [t] = [t]
concatTokens []  = []

parseCalendar :: Parser Token Calendar
parseCalendar = pack parseStartCal parseInsideCalendar parseEndCal

parseStartCal :: Parser Token Token
parseStartCal = satisfy (\(Token t) -> t == "BEGIN:VCALENDAR")

parseEndCal :: Parser Token Token
parseEndCal = satisfy (\(Token t) -> t == "END:VCALENDAR")

parseInsideCalendar :: Parser Token Calendar
parseInsideCalendar = Calendar <$> parseCalProp <*> parseEventList

parseCalProp :: Parser Token [Calprop]
parseCalProp = many $ parseProdID <<|> parseVersion

parseProdID :: Parser Token Calprop
parseProdID = (\(Token t) -> ProdID $ drop 7 t) <$> satisfy (\(Token t) -> "PRODID:" `isPrefixOf` t)

parseVersion :: Parser Token Calprop
parseVersion = (\(Token t) -> Version $ drop 8 t) <$> satisfy (\(Token t) -> "VERSION:" `isPrefixOf` t)

parseEventList :: Parser Token [Event]
parseEventList = many $ pack parseStartEvent parseEvent parseEndEvent

parseStartEvent :: Parser Token Token
parseStartEvent = satisfy (\(Token t) -> t == "BEGIN:VEVENT")

parseEndEvent :: Parser Token Token
parseEndEvent = satisfy (\(Token t) -> t == "END:VEVENT")

parseEvent :: Parser Token Event
parseEvent = Event <$> many parseEventProp

parseEventProp :: Parser Token EventProp
parseEventProp = parseUID <<|> parseDTStamp <<|> parseDTStart <<|> parseDTEnd <<|> parseDescription <<|> parseSummary <<|> parseLocation

parseUID :: Parser Token EventProp
parseUID = (\(Token t) -> UID $ drop 4 t) <$> satisfy (\(Token t) -> "UID:" `isPrefixOf` t)

parseDTStamp :: Parser Token EventProp
parseDTStamp = (\(Token t) -> DTStamp (fst $ head $ parse parseDateTime $ drop 8 t)) <$> satisfy (\(Token t) -> "DTSTAMP:" `isPrefixOf` t)

parseDTStart :: Parser Token EventProp
parseDTStart = (\(Token t) -> DTStart (fst $ head $ parse parseDateTime $ drop 8 t)) <$> satisfy (\(Token t) -> "DTSTART:" `isPrefixOf` t)

parseDTEnd :: Parser Token EventProp
parseDTEnd = (\(Token t) -> DTEnd (fst $ head $ parse parseDateTime $ drop 6 t)) <$> satisfy (\(Token t) -> "DTEND:" `isPrefixOf` t)

parseDescription :: Parser Token EventProp
parseDescription = (\(Token t) -> Description $ drop 12 t) <$> satisfy (\(Token t) -> "DESCRIPTION:" `isPrefixOf` t)

parseSummary :: Parser Token EventProp
parseSummary = (\(Token t) -> Summary $ drop 8 t) <$> satisfy (\(Token t) -> "SUMMARY:" `isPrefixOf` t)

parseLocation :: Parser Token EventProp
parseLocation = (\(Token t) -> Location $ drop 9 t) <$> satisfy (\(Token t) -> "LOCATION:" `isPrefixOf` t)

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined