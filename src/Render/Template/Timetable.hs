module Render.Template.Timetable where

import Lucid
import Control.Monad (forM_)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Clock (NominalDiffTime)
import Render.Lang (Lang, lShow)
import Render.Template.Ferry (DisplayTime, ferries_)
import Timetable (Timetable(..), Direction(..), FerryType(..), Ferry(..))


timetable_ :: DisplayTime t => Lang -> Monad m => Timetable t -> HtmlT m ()
timetable_ l (Timetable ferries day direction) = do
    h2_ (lShow l direction)
    ferries_ l ferries
