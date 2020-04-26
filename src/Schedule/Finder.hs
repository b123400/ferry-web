module Schedule.Finder where

import Data.Maybe (fromMaybe)
import Data.List (find, head, dropWhile)
import Data.Time.Calendar (Day, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Time.LocalTime (LocalTime(..), TimeZone, timeOfDayToTime, midnight, utcToLocalTime, localTimeToUTC, hoursToTimeZone)
import qualified Timetable as T
import Schedule.Calendar (HolidayCalendar, isHoliday)

-- An _infinite_ list of up coming ferries
ferriesForRouteAtTime :: HolidayCalendar -> T.Route NominalDiffTime -> LocalTime -> T.Direction -> [T.Ferry LocalTime]
ferriesForRouteAtTime calendar route@(T.Route _ timetables) lt@(LocalTime today _) direction =
    yesterdayLeftOvers <> todayFerries <> (futures tomorrow)
    where
        yesterdayLeftOvers :: [T.Ferry LocalTime]
        yesterdayLeftOvers =
            let yesterdayAll = fmap (localise yesterday) <$> allFerriesOfDay calendar route yesterday direction
            in dropWhile beforeNow yesterdayAll

        todayFerries :: [T.Ferry LocalTime]
        todayFerries = dropWhile beforeNow allFerriesOfToday

        allFerriesOfToday :: [T.Ferry LocalTime]
        allFerriesOfToday = fmap (localise today) <$> allFerriesOfDay calendar route today direction

        futures :: Day -> [T.Ferry LocalTime]
        futures fromDay =
            let today = (fmap (localise fromDay) <$> allFerriesOfDay calendar route fromDay direction)
            in if (null allFerries) -- If for some reason we cannot find any ferry (scraping issue?) stop the loop
               then today
               else today <> (futures $ addDays 1 fromDay)

        allFerries = timetables >>= T.ferries

        localise :: Day -> NominalDiffTime -> LocalTime
        localise day' diff = utcToLocalTime hongkongTimeZone
                            $ addUTCTime diff
                            $ localTimeToUTC hongkongTimeZone (LocalTime day' midnight)

        beforeNow (T.Ferry fTime _) = fTime < lt
        yesterday = addDays (-1) today
        tomorrow = addDays 1 today

hongkongTimeZone :: TimeZone
hongkongTimeZone = hoursToTimeZone 8

allFerriesOfDay :: HolidayCalendar -> T.Route t -> Day -> T.Direction -> [T.Ferry t]
allFerriesOfDay calendar route = ferriesOfDayAndDirection route . (toTimetableDay calendar)

ferriesOfDayAndDirection :: T.Route t -> T.Day -> T.Direction -> [T.Ferry t]
ferriesOfDayAndDirection (T.Route _ timetables) day direction =
    fromMaybe [] $ T.ferries <$> find matching timetables
    where matching (T.Timetable _ day' direction') = day == day' && direction == direction'


toTimetableDay :: HolidayCalendar -> Day -> T.Day
toTimetableDay calendar day
    | isHoliday calendar day = T.Holiday
    | otherwise =
        let (_, _, weekday) = toWeekDate day
        in case weekday of 6 -> T.Saturday
                           7 -> T.Sunday
                           _ -> T.Weekday
