module Render.Page.RawTimetable where

import Servant
import Lucid
import Control.Monad (forM_)
import Data.Aeson (ToJSON(..))
import Data.Time.Clock (NominalDiffTime)
import Render.Template.Wrapper (wrapper_)
import Render.Template.RawTimetable (rawTimetable_)
import Timetable (Route(..), Timetable(..), Day(..), islandName)

data RawTimetable = RawTimetable
    { route :: Route NominalDiffTime
    }


instance ToHtml RawTimetable where
    toHtmlRaw = toHtml
    toHtml (RawTimetable (Route island timetables)) = wrapper_ $ do
        div_ [class_ "island --detail"] $ do
            h1_ (toHtml $ islandName island)

            if isSatSameAsWeekday
                then do
                    h2_ "Monday - Saturday"
                    forM_ weekdayTimetables $ \t -> do
                        div_ [class_ "direction"] $ do
                            rawTimetable_ t
                else do
                    h2_ "Monday - Friday"
                    forM_ weekdayTimetables $ \t -> do
                        div_ [class_ "direction"] $ do
                            rawTimetable_ t
                    h2_ "Saturday"
                    forM_ saturdayTimetables $ \t -> do
                        div_ [class_ "direction"] $ do
                            rawTimetable_ t
            h2_ "Sunday"
            forM_ holidayTimetables $ \t -> do
                div_ [class_ "direction"] $ do
                    rawTimetable_ t
            div_ [class_ "clearfix"] $ pure ()

        where timetablesOfDay d = filter (\(Timetable _ day _)-> day == d) timetables
              weekdayTimetables = timetablesOfDay Weekday
              saturdayTimetables = timetablesOfDay Saturday
              holidayTimetables = timetablesOfDay SundayAndHoliday
              isSatSameAsWeekday = weekdayTimetables == saturdayTimetables


instance ToJSON RawTimetable where
    toJSON (RawTimetable route) = toJSON route
