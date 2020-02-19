module Render.Template.Timetable where

import Lucid
import Control.Monad (forM_)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Timetable (Timetable(..), Direction(..), FerryType(..), Ferry(..))

timetable_ :: Monad m => (Timetable LocalTime) -> HtmlT m ()
timetable_ (Timetable ferries day direction) = do
    h2_ directionName
    ol_ $ do
        forM_ ferries $ \(Ferry time ferryType)-> do
            li_ [class_ "timeslot"] $ do
                div_ [class_ (ferryClassName ferryType), alt_ (ferryAlt ferryType)] ""
                time_ (toHtml $ timeString time)
                span_ [class_ "reminder"] "In 1 hour" -- TODO
        li_ [class_ "more"] "More..."

    where
        directionName = case direction of
            ToIsland -> "To"
            FromIsland -> "From"

        ferryClassName ferryType = case ferryType of
            FastFerry -> "fast-ferry"
            SlowFerry -> "slow-ferry"
            OptionalFerry -> "optional-ferry"

        ferryAlt ferryType = case ferryType of
            FastFerry -> "Fast Ferry"
            SlowFerry -> "Slow Ferry"
            OptionalFerry -> "Optional Ferry"

        timeString (LocalTime _ (TimeOfDay hours minuties _)) =
            (twoDigits hours) <> ":" <> (twoDigits minuties)

        twoDigits num | num < 10 = "0" <> show num
                      | otherwise = show num
