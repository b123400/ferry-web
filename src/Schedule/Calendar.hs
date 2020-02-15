module Schedule.Calendar where

import Data.Time.Calendar (Day)

type HolidayCalendar = () -- TODO


isHoliday :: HolidayCalendar -> Day -> Bool
isHoliday _ _ = False