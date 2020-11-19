module Timetable.Class where

import Data.Proxy (Proxy)
import Data.Time.Clock (NominalDiffTime)
import Timetable (Island, Route)
import Timetable.Metadata (Metadata)

class HasTimetable m (i :: Island) where
    fetchTimetable :: (Proxy i) -> m (Route NominalDiffTime)

class HasMetadata m (i :: Island) where
    fetchMetadata :: (Proxy i) -> m Metadata
