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

data Day = Weekday | Saturday | Sunday | Holiday deriving (Show, Eq)

data Direction = ToIsland | FromIsland deriving (Show, Eq)

data Island = CentralCheungChau
            | CentralMuiWo
            | CentralPengChau
            | CentralYungShueWan
            | CentralSokKwuWan
            | NorthPointHungHom
            | NorthPointKowloonCity
            | PengChauHeiLingChau
            | AberdeenSokKwuWan
            | CentralDiscoveryBay
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
    parseUrlPiece "central-cheungchau" = Right CentralCheungChau
    parseUrlPiece "central-muiwo" = Right CentralMuiWo
    parseUrlPiece "central-pengchau" = Right CentralPengChau
    parseUrlPiece "central-yungshuewan" = Right CentralYungShueWan
    parseUrlPiece "central-sokkwuwan" = Right CentralSokKwuWan
    parseUrlPiece "northpoint-hunghom" = Right NorthPointHungHom
    parseUrlPiece "northpoint-kowlooncity" = Right NorthPointKowloonCity
    parseUrlPiece "pengchau-heilingchau" = Right PengChauHeiLingChau
    parseUrlPiece "aberdeen-sokkwuwan" = Right AberdeenSokKwuWan
    parseUrlPiece "central-discoverybay" = Right CentralDiscoveryBay
    parseUrlPiece _ = Left "Invalid island"

instance ToHttpApiData Island where
    toUrlPiece CentralCheungChau = "central-cheungchau"
    toUrlPiece CentralMuiWo = "central-muiwo"
    toUrlPiece CentralPengChau = "central-pengchau"
    toUrlPiece CentralYungShueWan = "central-yungshuewan"
    toUrlPiece CentralSokKwuWan = "central-sokkwuwan"
    toUrlPiece NorthPointHungHom = "northpoint-hunghom"
    toUrlPiece NorthPointKowloonCity  = "northpoint-kowlooncity"
    toUrlPiece PengChauHeiLingChau = "pengchau-heilingchau"
    toUrlPiece AberdeenSokKwuWan = "aberdeen-sokkwuwan"
    toUrlPiece CentralDiscoveryBay = "central-discoverybay"

islandName :: Island -> String
islandName i =
    case i of
        CentralCheungChau -> "Cheung Chau"
        CentralMuiWo -> "Mui Wo"
        CentralPengChau -> "Peng Chau"
        CentralYungShueWan -> "Yung Shue Wan"
        CentralSokKwuWan -> "Sok Kwu Wan"
        NorthPointHungHom -> "North Point -> Hung Hom"
        NorthPointKowloonCity -> "North Point -> Kowloon City"
        PengChauHeiLingChau -> "Peng Chau -> Hei Ling Chau"
        AberdeenSokKwuWan -> "Aberdeen -> Sok Kwu Wan"
        CentralDiscoveryBay -> "Central -> Discovery Bay"

limit :: Int -> Route t -> Route t
limit count (Route island timetables) = (Route island $ limit' <$> timetables)
    where
        limit' (Timetable fs day direction) = Timetable (take count fs) day direction

-- | Turns something like [2300, 0030] to [2300, 2430]
handleOverMidnight :: [Ferry NominalDiffTime] -> [Ferry NominalDiffTime]
handleOverMidnight [] = []
handleOverMidnight (onlyOne: []) = [onlyOne]
handleOverMidnight (p@(Ferry prev _): (Ferry curr t): rest) =
    let newCurr = if (prev > curr) then add1Day curr else curr
        newCurrFerry = Ferry newCurr t
    in (p: newCurrFerry: (tail  $ handleOverMidnight (newCurrFerry: rest)))
    where add1Day = (+) 86400

hongkongTimeZone :: TimeZone
hongkongTimeZone = hoursToTimeZone 8
