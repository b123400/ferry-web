module Render.Template.RawTimetable where

import Lucid
import Control.Monad (forM_)
import Data.Fixed (divMod')
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Clock (NominalDiffTime)
import Render.Lang (Lang, lShow)
import Render.Template.Ferry (DisplayTime, ferry_)
import Timetable (Timetable(..), Direction(..), Modifier(..), Ferry(..))


rawTimetable_ :: Monad m => Lang -> Timetable NominalDiffTime -> HtmlT m ()
rawTimetable_ l (Timetable ferries day direction) = do
    h2_ (lShow l direction)
    ol_ $ do
        forM_ ferries $ \f -> do
            ferry_ l $ timeToDaysAndTimeOfDay <$> f
    where
        timeToDaysAndTimeOfDay :: NominalDiffTime -> TimeOfDay
        timeToDaysAndTimeOfDay dt = let
            s = realToFrac dt
            (m,ms) = divMod' s 60
            (h,hm) = divMod' m 60
            in TimeOfDay h hm ms
