module Scraping.GovData.CentralYungShueWan
( fetch
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
import Scraping.GovData.Csv
import Scraping.GovData.TimeString (parseTimeStr)

csv :: (MonadIO m, MonadCache m ByteString) => m ByteString
csv = withCache "GovData-CentralYungShueWan-CSV" $
    simpleHttp "https://www.td.gov.hk/filemanager/en/content_1408/opendata/ferry_central_ysw_timetable_eng.csv"


fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "GovData-CentralYungShueWan" $ do
    res <- csv
    timetables <- case parseCsv @Entry' @EnumDays' res of
        Left err -> error err
        Right a -> pure a
    pure $ Route CentralYungShueWan timetables

data EnumDays' = MonToSat | SunAndHoliday

instance EnumDays EnumDays' where
    enumerated = [SunAndHoliday, MonToSat]
    toDaySet SunAndHoliday = sunAndHoliday
    toDaySet MonToSat = weekdaysAndSat


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
                               <*> m .: "Timetable"
                               <*> m .: "Remark"

instance FromField Direction' where
    parseField "Central to Yung Shue Wan" = pure $ Direction' FromPrimary
    parseField "Yung Shue Wan to Central" = pure $ Direction' ToPrimary
    parseField _ = fail "Cannot parse direction"

instance FromField Time' where
    parseField = fmap Time' . parseTimeStr . decodeUtf8

instance FromField Days' where
    parseField "Mondays to Saturdays except public holidays" = pure $ Days' weekdaysAndSat
    parseField "Sundays and public holidays" = pure $ Days' sunAndHoliday
    parseField _ = fail "Cannot parse Days"

instance FromField Remark' where
    -- 1 - denotes trip operated on Saturdays only
    parseField "1" = pure $ Remark'
        $ modifyDays (const $ singleton $ Weekday Saturday)
    -- 2 - denotes freight service is allowed
    parseField "2" = pure $ Remark'
        $ addModifier Freight
    -- 3 - denotes freight service is allowed and additional sailing may be operated subject to passenger demand
    parseField "3" = pure $ Remark'
        $ addModifier OptionalFerry
        . addModifier Freight
    parseField "" = pure $ Remark' id
    parseField _ = fail "Cannot parse remark"
