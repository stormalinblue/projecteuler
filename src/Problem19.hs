module Problem19 (
    solution
) where

nextMonth :: Int -> Int
nextMonth 11 = 0
nextMonth x = x + 1

nextYear :: Int -> Int
nextYear x = x + 1

nextDayOfWeek :: Int -> Int
nextDayOfWeek 6 = 0
nextDayOfWeek x = x + 1

nextDay :: Int -> Int -> Int
nextDay daysInMonth d = if d == daysInMonth - 1 then 0 else d + 1

monthDays :: Int -> Int -> Int
monthDays y 1 = if (y `rem` 4 == 0 && (y `rem` 100 /= 0 || y `rem` 400 == 0)) then 29 else 28
monthDays _ 8 = 30
monthDays _ 3 = 30
monthDays _ 5 = 30
monthDays _ 10 = 30
monthDays _ _ = 31

data Date = Date {year :: Int, month :: Int, day :: Int, weekday :: Int} deriving (Eq, Show)

nextDate :: Date -> Date
nextDate Date { year = y, month = m, day = d, weekday = w} =
    let
        nw = nextDayOfWeek w
        nd = nextDay (monthDays y m) d
        nm = if nd == 0 then nextMonth m else m
        ny = if nd == 0 && nm == 0 then nextYear y else y
    in Date {
        year = ny,
        month = nm,
        day = nd,
        weekday = nw
    }

takeWhileP1 :: (a -> Bool) -> [a] -> [a]
takeWhileP1 _ [] = []
takeWhileP1 p (x:xs) = if p x then [x] else x:takeWhileP1 p xs

startDay :: Date
startDay = Date { year = 1901, month = 0, day = 0, weekday = 1}
endDay :: Date
endDay = Date {year = 2000, month = 11, day = 30, weekday = 6}
dates :: [Date]
dates = iterate nextDate startDay
dateRange :: [Date]
dateRange = takeWhileP1 (== endDay) dates
filteredDates :: [Date]
filteredDates = filter (\x -> day x == 0 && weekday x == 6) dateRange

solution :: Int
solution = let
    countSundayAtMonthBegin = length filteredDates
    in countSundayAtMonthBegin