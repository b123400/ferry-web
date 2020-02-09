{-# LANGUAGE TemplateHaskell #-}

module Timetable where

import Data.Aeson.TH
import Data.Text (Text, pack, unpack, isInfixOf)
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay, LocalTime(..), hoursToTimeZone, utcToLocalTime)

data Timetable t = Timetable { ferries :: [Ferry t]
                             , day :: Day
                             , direction :: Direction
                             } deriving (Show)

data Ferry t = Ferry { time :: t
                     , ferryType :: FerryType
                     } deriving (Show)

data FerryType = FastFerry | SlowFerry | OptionalFerry deriving (Show)

data Day = Weekday | Saturday | Sunday | Holiday deriving (Show, Eq)

data Direction = ToIsland | FromIsland deriving (Show)

data Island = CheungChau
              deriving (Show)

data Route t = Route { island :: Island
                     , timetables :: [Timetable t]
                     } deriving (Show)

$(deriveJSON defaultOptions ''FerryType)
$(deriveJSON defaultOptions ''Ferry)
$(deriveJSON defaultOptions ''Day)
$(deriveJSON defaultOptions ''Timetable)
$(deriveJSON defaultOptions ''Direction)
$(deriveJSON defaultOptions ''Island)
$(deriveJSON defaultOptions ''Route)

instance Functor Route where
    fmap fn (Route i t) = Route i (fmap fn <$> t)

instance Functor Timetable where
    fmap fn (Timetable fs day direction) = Timetable (fmap fn <$> fs) day direction

instance Functor Ferry where
    fmap fn (Ferry time t) = Ferry (fn time) t

localise :: UTCTime -> TimeOfDay -> LocalTime
localise now inDay = LocalTime localDay inDay
    where (LocalTime localDay _) = utcToLocalTime hongkongTimeZone now
          hongkongTimeZone = hoursToTimeZone 8
