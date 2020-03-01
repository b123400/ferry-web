module Render.Template.Timetable where

import Lucid
import Control.Monad (forM_)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Clock (NominalDiffTime)
import Render.Template.Ferry (DisplayTime, ferries_)
import Timetable (Timetable(..), Direction(..), FerryType(..), Ferry(..))


timetable_ :: DisplayTime t => Monad m => Timetable t -> HtmlT m ()
timetable_ (Timetable ferries day direction) = do
    h2_ directionName
    ferries_ ferries
    where
        directionName = case direction of
            ToIsland -> "To"
            FromIsland -> "From"
