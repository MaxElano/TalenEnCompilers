module Features where

import DateTime
import Calendar


-- Exercise 9
countEvents :: Calendar -> Int
countEvents (Calendar _ events) = length events


findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ events) = filter (isDuring dt) events
  where
    isDuring :: DateTime -> Event -> Bool
    isDuring dt event =
      case (findDTStart event, findDTEnd event) of
        (Just l, Just h) -> betweenDT l h dt
        _                -> False

findDTStart :: Event -> Maybe DateTime
findDTStart (Event props) = case [dt | DTStart dt <- props] of
    [dt] -> Just dt
    _    -> Nothing

findDTEnd :: Event -> Maybe DateTime
findDTEnd (Event props) = case [dt | DTEnd dt <- props] of
    [dt] -> Just dt
    _    -> Nothing    

betweenDT :: DateTime -> DateTime -> DateTime -> Bool
betweenDT l h v = (l == v && h /= v) || (intL `lowerDT` intV && intV `lowerDT` intH)
    where 
        intL = intListDT l
        intH = intListDT h
        intV = intListDT v
          
intListDT :: DateTime -> [Int]
intListDT (DateTime (Date (Year dy) (Month dm) (Day dd)) (Time (Hour th) (Minute tm) (Second ts)) _) = [dy,dm,dd,th,tm,ts]

lowerDT :: [Int] -> [Int] -> Bool
lowerDT (l:ls) (h:hs) 
  | l < h     = True
  | l > h     = False
  | otherwise = lowerDT ls hs 
lowerDT _ _ = False


checkOverlapping :: Calendar -> Bool
checkOverlapping cal@(Calendar _ events) = any isOverlapping events
  where
    isOverlapping :: Event -> Bool
    isOverlapping event = 
        case (findDTStart event, findDTEnd event) of
            (Just l, Just h) -> anyOverlappingEvents l event || anyOverlappingEvents h event
            _                -> False

    anyOverlappingEvents :: DateTime -> Event -> Bool
    anyOverlappingEvents bound event = 
        let foundEvents = findEvents bound cal
        in  any (/= event) foundEvents



timeSpent :: String -> Calendar -> [Int]
timeSpent sum (Calendar _ events) = scanl (+) 0 (map eventMinutes (findSumEvents sum events))
  where 
    isSumInEvent :: String -> Event -> Bool
    isSumInEvent sum (Event props) = case [sum' | Summary sum' <- props] of
      [sum'] -> sum == sum'
      _      -> False

    findSumEvents :: String -> [Event] -> [Event]
    findSumEvents sum = filter (isSumInEvent sum)

    eventMinutes :: Event -> Int
    eventMinutes event = case (findDTStart event, findDTEnd event) of
        (Just l, Just h) -> difMinutes l h
        _                -> 0

    difMinutes :: DateTime -> DateTime -> Int
    difMinutes l h = floor (totalMinutes h - totalMinutes l)
    
    -- Approximation of total time
    totalMinutes :: DateTime -> Float
    totalMinutes 
      (DateTime (Date (Year dy) (Month dm) (Day dd)) (Time (Hour th) (Minute tm) (Second ts)) _)
      = (fromIntegral dy * 365 * 24 * 60)
      + ((fromIntegral dm-1) * 30.44 * 25 * 60)
      + ((fromIntegral dd-1) * 24 * 60)
      + (fromIntegral th * 60)
      + fromIntegral tm
      + fromIntegral ts / 60
      
