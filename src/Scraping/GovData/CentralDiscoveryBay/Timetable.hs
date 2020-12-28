module Scraping.GovData.CentralDiscoveryBay.Timetable
(
) where

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
csv = withCache "GovData-CentralDiscoveryBay-CSV" $
    simpleHttp "https://www.td.gov.hk/filemanager/en/content_1408/opendata/ferry_central_db_timetable_eng.csv"

instance (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => HasTimetable m CentralDiscoveryBay where
    fetchTimetable _ = withCache "GovData-CentralDiscoveryBay" $ do
        res <- csv
        timetables <- case parseCsv @Entry' @EnumDays' res of
            Left err -> error err
            Right a -> pure a
        pure $ Route CentralDiscoveryBay timetables

data EnumDays' = MonToFri | Sat | Sun | Holidays

instance EnumDays EnumDays' where
    enumerated = [Holidays, Sun, Sat, MonToFri]
    toDaySet Holidays = singleton $ Holiday
    toDaySet Sun = singleton $ Weekday Sunday
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
    parseField "Central to Discovery Bay" = pure $ Direction' FromPrimary
    parseField "Discovery Bay to Central" = pure $ Direction' ToPrimary
    parseField _ = fail "Cannot parse direction"

instance FromField Time' where
    parseField = fmap Time' . parseTimeStr . decodeUtf8

instance FromField Days' where
    parseField "Mondays to Fridays except public holidays" = pure $ Days' weekdays
    parseField "Saturdays except Public Holidays" = pure $ Days' (singleton $ Weekday Saturday)
    parseField "Sundays and public holidays" = pure $ Days' sunAndHoliday
    -- The actual csv is wrong with this value:
    parseField "Sundays and and Public Holidays" = pure $ Days' sunAndHoliday
    parseField _ = fail "Cannot parse Days"

instance FromField Remark' where
    -- 1 denotes The sailing will operate via Disneyland Resort Pier
    parseField "1" = pure $ Remark' id
    -- 2 denotes Operate on Sundays only
    parseField "2" = pure $ Remark'
        $ modifyDays (const $ singleton $ Weekday Sunday)
    parseField "" = pure $ Remark' id
    parseField _ = fail "Cannot parse remark"
