module Timetable.Class where

import Data.Proxy (Proxy)
import Data.Time.Clock (NominalDiffTime)
import Timetable (Island, Route)

class HasTimetable m (i :: Island) where
    fetchTimetable :: (Proxy i) -> m (Route NominalDiffTime)
