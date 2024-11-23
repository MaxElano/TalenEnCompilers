module Calendar where

import ParseLib
import DateTime


-- Exercise 6
data Calendar = Calendar { calprop :: Calprop
                         , eventList :: [Event] }
    deriving (Eq, Ord, Show)

data Calprop = Calprop { prodid  :: String 
                       , version :: String } --Always 2.0

data Event = Event { uid         :: String
                   , dtstamp     :: DateTime
                   , dtstart     :: DateTime
                   , dtend       :: DateTime
                   , description :: String
                   , summary     :: String
                   , location    :: String}
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
