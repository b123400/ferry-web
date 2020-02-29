module Timetable.Local where

import Control.Monad.IO.Class (MonadIO)
import Data.Cache (Cache)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Data.Traversable (for)
import Scraping.Calendar (holidayCalendar)
import Schedule.Finder (ferriesForRouteAtTime)
import Timetable (Route(..), Timetable(..), Day(..), Direction(..), Island)
import Timetable.Raw (allIslandsRaw, islandRaw')

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

islandAtTime :: Cache String (Route NominalDiffTime) -> Island -> LocalTime -> IO (Route LocalTime)
islandAtTime cache island time = do
    route <- islandRaw' cache island
    calendar <- holidayCalendar
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

addDiff :: LocalTime -> LocalTime -> (LocalTime, NominalDiffTime)
addDiff now lt = (lt, diffLocalTime lt now)
    where
        diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
        diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)
