module Scraping.GovData.CentralCheungChau.Timetable () where

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Csv (FromRecord(..), FromNamedRecord(..), FromField(..), (.:), (.!))
import Data.Set (Set, singleton, intersection)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (DayOfWeek(..))
import Data.Time.Clock (NominalDiffTime)
import Network.HTTP.Conduit (simpleHttp)


import Timetable hiding (timetables)
import Timetable.Class (HasTimetable(..))
import Scraping.GovData.Csv
import Scraping.GovData.TimeString (parseTimeStr)

csv :: (MonadIO m, MonadCache m ByteString) => m ByteString
csv = withCache "GovData-CentralCheungChau-CSV" $
    simpleHttp "https://www.td.gov.hk/filemanager/en/content_1408/opendata/ferry_central_cc_timetable_eng.csv"

instance (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => HasTimetable m CentralCheungChau where
    fetchTimetable _ = withCache "GovData-CentralCheungChau" $ do
        res <- csv
        timetables <- case parseCsv @Entry' @EnumDays' res of
            Left err -> error err
            Right a -> pure a
        pure $ Route CentralCheungChau timetables

data EnumDays' = MonToFri | Sat | SunAndHoliday

instance EnumDays EnumDays' where
    enumerated = [SunAndHoliday, Sat, MonToFri]
    toDaySet SunAndHoliday = sunAndHoliday
    toDaySet Sat = singleton $ Weekday Saturday
    toDaySet MonToFri = weekdays


newtype Direction' = Direction' Direction
newtype Days' = Days' (Set Day) deriving (Show)
newtype Time' = Time' NominalDiffTime
newtype Remark' = Remark' Remark

type Entry' = Entry Direction' Days' Time' Remark'

instance FromRecord Entry' where
    parseRecord v
        | length v == 4 = Entry <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
        | otherwise     = mzero

instance FromNamedRecord Entry' where
    parseNamedRecord m = Entry <$> m .: "Direction"
                               <*> m .: "Service Date"
                               <*> m .: "Service Hour"
                               <*> m .: "Remark"

instance FromField Direction' where
    parseField "Central to Cheung Chau" = pure $ Direction' FromPrimary
    parseField "Cheung Chau to Central" = pure $ Direction' ToPrimary
    parseField _ = fail "Cannot parse direction"

instance FromField Time' where
    parseField = fmap Time' . parseTimeStr . decodeUtf8

instance FromField Days' where
    parseField "Mondays to Saturdays except public holidays" = pure $ Days' weekdaysAndSat
    parseField "Sundays and public holidays" = pure $ Days' sunAndHoliday
    parseField _ = fail "Cannot parse Days"

instance FromField Remark' where
    -- 1 - denotes Ordinary ferry service and freight service is allowed
    parseField "1" = pure $ Remark'
        $ addModifier SlowFerry
        . addModifier Freight
    -- 2 - denotes trip operated on Mondays to Fridays only (except public holidays)
    parseField "2" = pure $ Remark'
        $ addModifier FastFerry
        . modifyDays (intersection weekdays)
    -- 3 - denotes Ordinary ferry service and freight service is allowed and trip operated on Saturdays only (except public holidays)
    parseField "3" = pure $ Remark'
        $ addModifier SlowFerry
        . addModifier Freight
        . modifyDays (const $ singleton $ Weekday Saturday)
    -- 4 - denotes Ordinary ferry service and freight service is allowed and trip operated on Mondays to Fridays only (except public holidays)
    parseField "4" = pure $ Remark'
        $ addModifier SlowFerry
        . addModifier Freight
        . modifyDays (intersection weekdays)
    parseField "" = pure $ Remark'
        $ addModifier FastFerry
    parseField _ = fail "Cannot parse remark"
