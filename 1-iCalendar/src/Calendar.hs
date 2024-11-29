{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Calendar where

import ParseLib
import DateTime
import Data.Char
import Data.List


-- Exercise 6
data Calendar = Calendar { calpropList    :: [Calprop]
                         , eventList      :: [Event] }
    deriving (Eq, Ord, Show)

data Calprop
    = PropID String
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

-- Divides the strings at the crlf points and takes everything that is seperated by the crlf points
lexCalendar :: Parser Char [Token]
lexCalendar = concatTokens . filterTokens <$> listOf (Token <$> many (satisfy (/= '\r'))) (ParseLib.token "\r\n")

-- Filters any tokens that are empty out
filterTokens :: [Token] -> [Token]
filterTokens = filter (not . (\(Token t) -> null t))

-- Concats two tokens if the second one starts with a space as those two should be one token
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
parseProdID = (\(Token t) -> PropID $ drop 7 t) <$> satisfy (\(Token t) -> "PRODID:" `isPrefixOf` t)

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

parseDTStamp :: Parser Token EventProp
parseDTStamp = (\(Token t) -> DTStamp (fst $ head $ parse parseDateTime $ drop 8 t)) <$> satisfy (\(Token t) -> "DTSTAMP:" `isPrefixOf` t)

parseUID :: Parser Token EventProp
parseUID = (\(Token t) -> UID $ drop 4 t) <$> satisfy (\(Token t) -> "UID:" `isPrefixOf` t)

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
printCalendar (Calendar calprops events) = "BEGIN:VCALENDAR\r\n" ++ printCalProps calprops ++ printEvents events ++ "END:VCALENDAR\r\n"

printCalProps :: [Calprop] -> String
printCalProps = concatMap printCalProp

printCalProp :: Calprop -> String
printCalProp (PropID propid)   = "PROPID:"  ++ propid  ++ "\r\n"
printCalProp (Version version) = "VERSION:" ++ version ++ "\r\n"

printEvents :: [Event] -> String
printEvents = concatMap printEvent

printEvent :: Event -> String
printEvent (Event eventProps) = "BEGIN:VEVENT\r\n" ++ printEventProps eventProps ++ "END:VEVENT\r\n"

printEventProps :: [EventProp] -> String
printEventProps = concatMap printEventProp

printEventProp :: EventProp -> String
printEventProp (DTStamp dt)      = "DTSTAMP:"     ++ printDateTime dt ++ "\r\n"
printEventProp (UID id)          = "UID:"         ++ id               ++ "\r\n"
printEventProp (DTStart dt)      = "DTSTART:"     ++ printDateTime dt ++ "\r\n"
printEventProp (DTEnd dt)        = "DTEND:"       ++ printDateTime dt ++ "\r\n"
printEventProp (Description dsc) = "DESCRIPTION:" ++ dsc              ++ "\r\n"
printEventProp (Summary sum)     = "SUMMARY:"     ++ sum              ++ "\r\n"
printEventProp (Location loc)    = "LOCATION:"    ++ loc              ++ "\r\n"