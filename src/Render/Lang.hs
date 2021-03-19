module Render.Lang where

import Control.Applicative ((<|>))
import Data.ByteString.Char8 (ByteString, split, unpack)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString)
import Servant
import Text.Read (readMaybe)
import Network.HTTP.Media (mapAcceptLanguage)
import Web.Cookie (SetCookie, setCookieValue, setCookieName)


data Lang = En | Hk deriving (Show, Read)

instance FromHttpApiData Lang where
    parseUrlPiece "en" = Right En
    parseUrlPiece "hk" = Right Hk
    parseUrlPiece _ = Left "Not supported"
    parseHeader = Right . fromMaybe Hk . mapAcceptLanguage
          [ ("en", En)
          , ("yue-HK", Hk)
          , ("zh-HK", Hk)
          , ("zh-yue-HK", Hk)
          , ("zh-Hant", Hk)
          ]

instance FromHttpApiData [SetCookie] where
    parseUrlPiece _ = Left "Not supported"
    parseHeader bs = Right $ mapMaybe rightOnly (parseHeader <$> split ';' bs)
        where
            rightOnly (Right a) = Just a
            rightOnly _ = Nothing

withLang :: (Lang -> a) -> Maybe Lang -> Maybe [SetCookie] -> a
withLang fn mLang cookie = fn (fromMaybe Hk $ mCookieLang <|> mLang)
    where mCookieLang = (readMaybe . unpack) =<< (setCookieValue <$> (find ((==) "lang" . setCookieName) $ fromMaybe [] cookie))

translate :: IsString s => Lang -> Syllabus -> s
translate En = en
translate Hk = hk

class LocalisedShow a where
    lShow :: IsString s => Lang -> a -> s

data Localised a = Localised Lang a

data Syllabus
    -- Detail page
    = Now
    | SelectDate
    | ItemsPerPage
    | Submit
    | RawTimetable
    | DataSource
    | Wiki
    | Metadata
    -- Raw timetable page
    | MondayToSaturday
    | MondayToFriday
    | Saturday
    | Sunday
    | Holiday
    -- Metadata page
    | DeluxeClass
    | SlowFerry
    | FastFerry
    | Passenger
    | Fare
    | Type
    | Extras
    | Duration
    | Minutes

en :: IsString s => Syllabus -> s
en Now = "Now"
en SelectDate = "Select Date"
en ItemsPerPage = "Items per page"
en Submit = "Submit"
en RawTimetable = "Raw Timetable"
en MondayToSaturday = "Monday - Saturday"
en MondayToFriday = "Monday - Friday"
en Saturday = "Saturday"
en Sunday = "Sunday"
en Holiday = "Holiday"
en DataSource = "Data source"
en Wiki = "EFHK"
en Metadata = "Fares and Durations"
en DeluxeClass = "Deluxe Class"
en SlowFerry = "Slow Ferry"
en FastFerry = "Fast Ferry"
en Passenger = "Passenger"
en Fare = "Fare"
en Type = "Type"
en Extras = "Extras"
en Duration = "Duration"
en Minutes = "Minutes"

hk :: IsString s => Syllabus -> s
hk Now = "現在"
hk SelectDate = "選擇日期"
hk ItemsPerPage = "每頁數目"
hk Submit = "提交"
hk RawTimetable = "船期表"
hk MondayToSaturday = "星期一至六"
hk MondayToFriday = "星期一至五"
hk Saturday = "星期六"
hk Sunday = "星期日"
hk Holiday = "假期"
hk DataSource = "資料來源"
hk Wiki = "香港渡輪大典"
hk Metadata = "收費與船程"
hk DeluxeClass = "豪華位"
hk SlowFerry = "慢船"
hk FastFerry = "快船"
hk Passenger = "乘客"
hk Fare = "船費"
hk Type = "種類"
hk Extras = "其他"
hk Duration = "需時"
hk Minutes = "分鐘"
