module Render.Template.RawTimetable where

import Lucid
import Control.Monad (forM_)
import Data.Fixed (divMod')
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Clock (NominalDiffTime)
import Render.Template.Ferry (DisplayTime, ferry_)
import Timetable (Timetable(..), Direction(..), FerryType(..), Ferry(..))


rawTimetable_ :: Monad m => Timetable NominalDiffTime -> HtmlT m ()
rawTimetable_ (Timetable ferries day direction) = do
    h2_ directionName
    ol_ $ do
        forM_ ferries $ \f -> do
            ferry_ $ timeToDaysAndTimeOfDay <$> f
    where
        directionName = case direction of
            ToIsland -> "To"
            FromIsland -> "From"

        timeToDaysAndTimeOfDay :: NominalDiffTime -> TimeOfDay
        timeToDaysAndTimeOfDay dt = let
            s = realToFrac dt
            (m,ms) = divMod' s 60
            (h,hm) = divMod' m 60
            in TimeOfDay h hm ms
