module Timetable.Local where

import Control.Monad.IO.Class (MonadIO)
import Data.Cache (Cache)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Data.Traversable (for)
import Scraping.Calendar (holidayCalendar)
import Schedule.Finder (ferriesForRouteAtTime)
import Timetable (Route(..), Timetable(..), Day(..), Direction(..))
import Timetable.Raw (allIslandsRaw)

allIslandsAtTime :: Cache String (Route NominalDiffTime) -> LocalTime -> IO [Route LocalTime]
allIslandsAtTime cache time = do
    routes <- allIslandsRaw cache
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

addDiff :: LocalTime -> LocalTime -> (LocalTime, NominalDiffTime)
addDiff now lt = (lt, diffLocalTime lt now)
    where
        diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
        diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)
