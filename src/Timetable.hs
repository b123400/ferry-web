{-# LANGUAGE TemplateHaskell #-}

module Timetable where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(String), (.=), (.:), object, withObject, withText)
import Data.Aeson.TH
import Data.Set (Set, insert, fromList)
import Data.String (IsString, fromString)
import Data.Time.Calendar (DayOfWeek(..))
import Data.Time.Clock (NominalDiffTime)
import Data.Time.LocalTime (LocalTime(..), ZonedTime(..), TimeZone, hoursToTimeZone)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Render.Lang (Lang(..), LocalisedShow(..))
import qualified Data.Map.Strict as M

data Timetable t = Timetable { ferries :: [Ferry t]
                             , days :: Set Day
                             , direction :: Direction
                             } deriving (Eq, Show)

data Ferry t = Ferry { time :: t
                     , modifiers :: Set Modifier
                     } deriving (Eq, Show)

data Modifier = FastFerry | SlowFerry | OptionalFerry deriving (Eq, Ord, Show)

data Day = Weekday DayOfWeek | Holiday deriving (Show, Eq)

data Direction = FromPrimary | ToPrimary deriving (Show, Eq)

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

$(deriveJSON defaultOptions ''Modifier)
$(deriveJSON defaultOptions ''Ferry)
$(deriveJSON defaultOptions ''Direction)
$(deriveJSON defaultOptions ''Island)

instance Enum Day where
    toEnum 8 = Holiday
    toEnum n | n >= 1 && n <= 7 = Weekday $ toEnum n
    toEnum _ = error "Not supported day"
    fromEnum Holiday = 8
    fromEnum (Weekday d) = fromEnum d

instance Ord Day where
    compare d1 d2
        | d1 == d2 = EQ
        | otherwise = compare (fromEnum d1) (fromEnum d2)

instance ToJSON Day where
    toJSON Holiday = "holiday"
    toJSON (Weekday d) = toJSON d

instance FromJSON Day where
    parseJSON = withText "Day" $ \t ->
        case t of
            "holiday" -> pure Holiday
            x -> Weekday <$> parseJSON (String x)

instance ToJSON (Timetable LocalTime) where
    toJSON (Timetable ferries _ direction) = object
        [ "ferries" .= (fmap zoned <$> ferries)
        , "direction" .= direction
        ]
        where zoned = flip ZonedTime hongkongTimeZone


instance ToJSON (Timetable NominalDiffTime) where
    toJSON (Timetable ferries days direction) = object
        [ "ferries" .= ferries
        , "days" .= days
        , "direction" .= direction
        ]

instance FromJSON (Timetable NominalDiffTime) where
    parseJSON = withObject "Timetable" $ \o -> do
        ferries <- o .: "ferries"
        days <- o .: "days"
        direction <- o .: "direction"
        pure $ Timetable ferries days direction

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
    lShow En FromPrimary = "To"
    lShow En ToPrimary = "From"
    lShow Hk FromPrimary = "去程"
    lShow Hk ToPrimary = "回程"

instance LocalisedShow Day where
    lShow En (Weekday x) = fromString $ show x
    lShow En Holiday = "Holiday"
    lShow Hk (Weekday Monday) = "星期一"
    lShow Hk (Weekday Tuesday) = "星期二"
    lShow Hk (Weekday Wednesday) = "星期三"
    lShow Hk (Weekday Thursday) = "星期四"
    lShow Hk (Weekday Friday) = "星期五"
    lShow Hk (Weekday Saturday) = "星期六"
    lShow Hk (Weekday Sunday) = "星期日"
    lShow Hk Holiday = "公眾假期"


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

weekdays :: Set Day
weekdays = fromList $ Weekday <$>
    [ Monday
    , Tuesday
    , Wednesday
    , Thursday
    , Friday
    ]

weekdaysAndSat :: Set Day
weekdaysAndSat = insert (Weekday Saturday) weekdays

everyday :: Set Day
everyday = insert (Weekday Sunday) weekdaysAndSat

sunAndHoliday :: Set Day
sunAndHoliday = fromList $ [Weekday Sunday, Holiday]

satSunAndHoliday :: Set Day
satSunAndHoliday = insert (Weekday Saturday) sunAndHoliday

groupByDays :: [Timetable t] -> [(Set Day, [Timetable t])]
groupByDays = M.toList . foldl f mempty
    where f m timetable@(Timetable _ days _) = M.insertWith (<>) days [timetable] m

dataSource :: IsString s => Island -> s
dataSource CentralCheungChau = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o01"
dataSource CentralMuiWo = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o02"
dataSource CentralPengChau = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o03"
dataSource CentralYungShueWan = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o04"
dataSource CentralSokKwuWan = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o05"
dataSource NorthPointHungHom = "http://www.nwff.com.hk/route/get_route.php?id=1c87d6ed-4ace-464c-b24e-4db2b83ce902&route_id=5"
dataSource NorthPointKowloonCity  = "http://www.nwff.com.hk/route/get_route.php?id=6662173e-a9ee-489b-aa36-8ce56dec0a6b&route_id=6&submenu_num=3"
dataSource PengChauHeiLingChau = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o03"
dataSource AberdeenSokKwuWan = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o10"
dataSource CentralDiscoveryBay = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o11"
dataSource MaWanTsuenWan = "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/index.html#o17"
dataSource SaiWanHoKwunTong = "https://www.coralseaferryservice.com.hk/timetable"
dataSource SaiWanHoSamKaTsuen = "https://www.coralseaferryservice.com.hk/timetable"
dataSource SamKaTsuenTungLungIsland = "https://www.coralseaferryservice.com.hk/timetable"
