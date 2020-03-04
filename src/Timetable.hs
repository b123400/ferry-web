{-# LANGUAGE TemplateHaskell #-}

module Timetable where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import Data.Aeson.TH
import Data.Time.Clock (NominalDiffTime)
import Data.Time.LocalTime (LocalTime(..), ZonedTime(..), TimeZone, hoursToTimeZone)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

data Timetable t = Timetable { ferries :: [Ferry t]
                             , day :: Day
                             , direction :: Direction
                             } deriving (Eq, Show)

data Ferry t = Ferry { time :: t
                     , ferryType :: FerryType
                     } deriving (Eq, Show)

data FerryType = FastFerry | SlowFerry | OptionalFerry deriving (Eq, Show)

data Day = Weekday | Saturday | SundayAndHoliday deriving (Show, Eq)

data Direction = ToIsland | FromIsland deriving (Show, Eq)

data Island = CheungChau
            | MuiWo
            | PengChau
            | YungShueWan
            | SokKwuWan
              deriving (Eq, Show)

data Route t = Route { island :: Island
                     , timetables :: [Timetable t]
                     } deriving (Show)

$(deriveJSON defaultOptions ''FerryType)
$(deriveJSON defaultOptions ''Ferry)
$(deriveJSON defaultOptions ''Day)
$(deriveJSON defaultOptions ''Direction)
$(deriveJSON defaultOptions ''Island)

instance ToJSON (Timetable LocalTime) where
    toJSON (Timetable ferries _ direction) = object
        [ "ferries" .= (fmap zoned <$> ferries)
        , "direction" .= direction
        ]
        where zoned = flip ZonedTime hongkongTimeZone


instance ToJSON (Timetable NominalDiffTime) where
    toJSON (Timetable ferries day direction) = object
        [ "ferries" .= ferries
        , "day" .= day
        , "direction" .= direction
        ]

instance FromJSON (Timetable NominalDiffTime) where
    parseJSON = withObject "Timetable" $ \o -> do
        ferries <- o .: "ferries"
        day <- o .: "day"
        direction <- o .: "direction"
        pure $ Timetable ferries day direction

instance (ToJSON (Timetable t)) => ToJSON (Route t) where
    toJSON (Route island timetables) = object
        [ "island" .= island
        , "timetables" .= timetables
        ]

instance FromJSON (Route NominalDiffTime) where
    parseJSON = withObject "Route" $ \o -> do
        island <- o .: "island"
        timetables <- o .: "timetables"
        pure $ Route island timetables

instance Functor Route where
    fmap fn (Route i t) = Route i (fmap fn <$> t)

instance Functor Timetable where
    fmap fn (Timetable fs day direction) = Timetable (fmap fn <$> fs) day direction

instance Functor Ferry where
    fmap fn (Ferry time t) = Ferry (fn time) t

instance FromHttpApiData Island where
    parseUrlPiece "cheungchau" = Right CheungChau
    parseUrlPiece "muiwo" = Right MuiWo
    parseUrlPiece "pengchau" = Right PengChau
    parseUrlPiece "yungshuewan" = Right YungShueWan
    parseUrlPiece "sokkwuwan" = Right SokKwuWan
    parseUrlPiece _ = Left "Invalid island"

instance ToHttpApiData Island where
    toUrlPiece CheungChau = "cheungchau"
    toUrlPiece MuiWo = "muiwo"
    toUrlPiece PengChau = "pengchau"
    toUrlPiece YungShueWan = "yungshuewan"
    toUrlPiece SokKwuWan = "sokkwuwan"

islandName :: Island -> String
islandName i =
    case i of
        CheungChau -> "Cheung Chau"
        MuiWo -> "Mui Wo"
        PengChau -> "Peng Chau"
        YungShueWan -> "Yung Shue Wan"
        SokKwuWan -> "Sok Kwu Wan"

limit :: Int -> Route t -> Route t
limit count (Route island timetables) = (Route island $ limit' <$> timetables)
    where
        limit' (Timetable fs day direction) = Timetable (take count fs) day direction

hongkongTimeZone :: TimeZone
hongkongTimeZone = hoursToTimeZone 8
