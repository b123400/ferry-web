module Timetable.Local where

import Control.Monad.IO.Class (MonadIO)
import Data.Time.LocalTime (LocalTime)
import Data.Traversable (for)
import Scraping.Calendar (holidayCalendar)
import Schedule.Finder (ferriesForRouteAtTime)
import Timetable (Route(..), Timetable(..), Day(..), Direction(..))
import Timetable.Raw (allIslandsRaw)

allIslandsAtTime :: LocalTime -> IO [Route LocalTime]
allIslandsAtTime time = do
    routes <- allIslandsRaw
    calendar <- holidayCalendar
    for routes $ \route@(Route island timetable)-> do
        pure $ Route island
            [ Timetable { ferries = ferriesForRouteAtTime calendar route time FromIsland
                        , day = Weekday -- Useless
                        , direction = FromIsland
                        }
            , Timetable { ferries = ferriesForRouteAtTime calendar route time ToIsland
                        , day = Weekday -- Useless
                        , direction = ToIsland
                        }
            ]

-- TODO
-- islandAtTime :: Proxy i -> LocalTime -> IO [Timetable LocalTime]
