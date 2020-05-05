{-# LANGUAGE TemplateHaskell #-}

module Timetable where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import Data.Aeson.TH
import Data.String (IsString, fromString)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.LocalTime (LocalTime(..), ZonedTime(..), TimeZone, hoursToTimeZone)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Render.Lang (Lang(..), LocalisedShow(..))

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
            | MaWanTsuenWan
            | SaiWanHoKwunTong
            | SaiWanHoSamKaTsuen
            | SamKaTsuenTungLungIsland
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
        [ "island" .= toUrlPiece island
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
    parseUrlPiece "mawan-tsuenwawn" = Right MaWanTsuenWan
    parseUrlPiece "saiwanho-kwuntong" = Right SaiWanHoKwunTong
    parseUrlPiece "saiwanho-samkatsuen" = Right SaiWanHoSamKaTsuen
    parseUrlPiece "samkatsuen-tunglungisland" = Right SamKaTsuenTungLungIsland
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
    toUrlPiece MaWanTsuenWan = "mawan-tsuenwawn"
    toUrlPiece SaiWanHoKwunTong = "saiwanho-kwuntong"
    toUrlPiece SaiWanHoSamKaTsuen = "saiwanho-samkatsuen"
    toUrlPiece SamKaTsuenTungLungIsland = "samkatsuen-tunglungisland"

primaryName :: IsString s => Lang -> Island -> s
primaryName En i =
    case i of
        CentralCheungChau -> "Central"
        CentralMuiWo -> "Central"
        CentralPengChau -> "Central"
        CentralYungShueWan -> "Central"
        CentralSokKwuWan -> "Central"
        NorthPointHungHom -> "NorthPoint"
        NorthPointKowloonCity -> "NorthPoint"
        PengChauHeiLingChau -> "PengChau"
        AberdeenSokKwuWan -> "Aberdeen"
        CentralDiscoveryBay -> "Central"
        MaWanTsuenWan -> "Ma Wan"
        SaiWanHoKwunTong -> "Sai Wan Ho"
        SaiWanHoSamKaTsuen -> "Sai Wan Ho"
        SamKaTsuenTungLungIsland -> "Sam Ka Tsuen"
primaryName Hk i =
    case i of
        CentralCheungChau -> "中環"
        CentralMuiWo -> "中環"
        CentralPengChau -> "中環"
        CentralYungShueWan -> "中環"
        CentralSokKwuWan -> "中環"
        NorthPointHungHom -> "北角"
        NorthPointKowloonCity -> "北角"
        PengChauHeiLingChau -> "坪洲"
        AberdeenSokKwuWan -> "香港仔"
        CentralDiscoveryBay -> "中環"
        MaWanTsuenWan -> "馬灣"
        SaiWanHoKwunTong -> "西灣河"
        SaiWanHoSamKaTsuen -> "西灣河"
        SamKaTsuenTungLungIsland -> "三家村"

secondaryName En i =
    case i of
        CentralCheungChau -> "Cheung Chau"
        CentralMuiWo -> "Mui Wo"
        CentralPengChau -> "Peng Chau"
        CentralYungShueWan -> "Yung Shue Wan"
        CentralSokKwuWan -> "Sok Kwu Wan"
        NorthPointHungHom -> "Hung Hom"
        NorthPointKowloonCity -> "Kowloon City"
        PengChauHeiLingChau -> "Hei Ling Chau"
        AberdeenSokKwuWan -> "Sok Kwu Wan"
        CentralDiscoveryBay -> "Discovery Bay"
        MaWanTsuenWan -> "Tsuen Wan"
        SaiWanHoKwunTong -> "Kwun Tong"
        SaiWanHoSamKaTsuen -> "Sam Ka Tsuen"
        SamKaTsuenTungLungIsland -> "Tung Lung Island"

secondaryName Hk i =
    case i of
        CentralCheungChau -> "長洲"
        CentralMuiWo -> "梅窩"
        CentralPengChau -> "坪洲"
        CentralYungShueWan -> "榕樹灣"
        CentralSokKwuWan -> "索罟灣"
        NorthPointHungHom -> "紅磡"
        NorthPointKowloonCity -> "九龍城"
        PengChauHeiLingChau -> "喜靈洲"
        AberdeenSokKwuWan -> "索罟灣"
        CentralDiscoveryBay -> "愉景灣"
        MaWanTsuenWan -> "荃灣"
        SaiWanHoKwunTong -> "觀塘"
        SaiWanHoSamKaTsuen -> "三家村"
        SamKaTsuenTungLungIsland -> "東龍洲"

instance LocalisedShow Island where
    lShow lang i = fromString $ (fromString $ primaryName lang i) <> " - " <> (fromString $ secondaryName lang i)

instance LocalisedShow Direction where
    lShow En ToIsland = "To"
    lShow En FromIsland = "From"
    lShow Hk ToIsland = "去程"
    lShow Hk FromIsland = "回程"

limit :: Int -> Route t -> Route t
limit count (Route island timetables) = (Route island $ limit' <$> timetables)
    where
        limit' (Timetable fs day direction) = Timetable (take count fs) day direction

takeUntil :: LocalTime -> Route LocalTime -> Route LocalTime
takeUntil targetTime (Route island timetables) = (Route island $ until <$> timetables)
    where
        until (Timetable fs day direction) = Timetable (takeWhile ((< targetTime) . time) fs) day direction

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
