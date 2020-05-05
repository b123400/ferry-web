module Render.Page.RawTimetable where

import Servant
import Lucid
import Control.Monad (forM_)
import Data.Aeson (ToJSON(..))
import Data.Time.Clock (NominalDiffTime)
import Render.Lang as L (Localised(..), translate, lShow)
import qualified Render.Lang as L
import Render.Template.Wrapper (wrapper_)
import Render.Template.RawTimetable (rawTimetable_)
import Timetable (Route(..), Timetable(..), Day(..))

data RawTimetable = RawTimetable
    { route :: Route NominalDiffTime
    }

instance ToHtml (Localised RawTimetable) where
    toHtmlRaw = toHtml
    toHtml (Localised l (RawTimetable (Route island timetables))) = wrapper_ l $ do
        div_ [class_ "island --raw"] $ do
            h1_ (lShow l island)

            if isSatSameAsWeekday
                then do
                    div_ [class_ "day"] $ do
                        h2_ (t L.MondayToSaturday)
                        forM_ weekdayTimetables $ \t -> do
                            div_ [class_ "direction"] $ do
                                rawTimetable_ l t
                            div_ [class_ "clearfix"] $ pure ()
                else do
                    div_ [class_ "day"] $ do
                        h2_ (t L.MondayToFriday)
                        forM_ weekdayTimetables $ \t -> do
                            div_ [class_ "direction"] $ do
                                rawTimetable_ l t
                        div_ [class_ "clearfix"] $ pure ()
                    div_ [class_ "day"] $ do
                        h2_ (t L.Saturday)
                        forM_ saturdayTimetables $ \t -> do
                            div_ [class_ "direction"] $ do
                                rawTimetable_ l t
                        div_ [class_ "clearfix"] $ pure ()
            -- TODO: join Sunday and holiday is possible
            div_ [class_ "day"] $ do
                h2_ (t L.Sunday)
                forM_ sundayTimetables $ \t -> do
                    div_ [class_ "direction"] $ do
                        rawTimetable_ l t
                div_ [class_ "clearfix"] $ pure ()
            div_ [class_ "day"] $ do
                h2_ (t L.Holiday)
                forM_ holidayTimetables $ \t -> do
                    div_ [class_ "direction"] $ do
                        rawTimetable_ l t
                div_ [class_ "clearfix"] $ pure ()
            div_ [class_ "clearfix"] $ pure ()

        where timetablesOfDay d = filter (\(Timetable _ day _)-> day == d) timetables
              weekdayTimetables = timetablesOfDay Weekday
              saturdayTimetables = timetablesOfDay Saturday
              sundayTimetables = timetablesOfDay Sunday
              holidayTimetables = timetablesOfDay Holiday
              isSatSameAsWeekday = weekdayTimetables == saturdayTimetables
              t = translate l


instance ToJSON (Localised RawTimetable) where
    toJSON (Localised l (RawTimetable route)) = toJSON route
