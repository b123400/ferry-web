module Timetable.Local where

import Control.Monad.Cache (MonadCache)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Data.Traversable (for)
import Scraping.Calendar (holidayCalendar)
import Schedule.Calendar (HolidayCalendar)
import Schedule.Finder (ferriesForRouteAtTime)
import Timetable (Route(..), Timetable(..), Day(..), Direction(..), Island)
import Timetable.Raw (allIslandsRaw, islandRaw)

allIslandsAtTime
    :: ( MonadIO m
       , MonadCache m ByteString
       , MonadCache m (Route NominalDiffTime)
       , MonadCache m HolidayCalendar
       , MonadCatch m
       )
    => LocalTime -> m [Route LocalTime]
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

islandAtTime
    :: ( MonadIO m
       , MonadCache m ByteString
       , MonadCache m (Route NominalDiffTime)
       , MonadCache m HolidayCalendar
       , MonadCatch m
        )
    => Island -> LocalTime -> m (Route LocalTime)
islandAtTime island time = do
    route <- islandRaw island
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
