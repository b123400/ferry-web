module Scraping.GovData.CentralPengChau.Timetable
(
) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Csv (Parser, FromRecord(..), FromNamedRecord(..), FromField(..), (.:), (.!))
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
csv = withCache "GovData-CentralPengChau-CSV" $
    simpleHttp "https://www.td.gov.hk/filemanager/en/content_1408/opendata/ferry_central_pc_timetable_eng.csv"

instance (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => HasTimetable m CentralPengChau where
    fetchTimetable _ = withCache "GovData-CentralPengChau" $ do
        res <- csv
        timetables <- case parseCsv' @Entry' @EnumDays' (tryFirstAsDirection @Direction') res of
            Left err -> error err
            Right a -> pure a
        pure $ Route CentralPengChau timetables

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

instance FromField Direction' where
    parseField "Central to Peng Chau" = pure $ Direction' FromPrimary
    parseField "Peng Chau to Central" = pure $ Direction' ToPrimary
    parseField _ = fail "Cannot parse direction"

instance FromField Time' where
    parseField = fmap Time' . parseTimeStr . decodeUtf8

instance FromField Days' where
    parseField "Mondays to Saturdays except public holidays" = pure $ Days' weekdaysAndSat
    parseField "Sundays and public holidays" = pure $ Days' sunAndHoliday
    parseField _ = fail "Cannot parse Days"

instance FromField Remark' where
    -- 1 - denotes ordinary ferry service and freight service is allowed
    parseField "1" = pure $ Remark'
        $ addModifier Freight
        . addModifier SlowFerry
    parseField "" = pure $ Remark'
        $ addModifier FastFerry
    parseField _ = fail "Cannot parse remark"
