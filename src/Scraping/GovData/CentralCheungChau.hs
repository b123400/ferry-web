module Scraping.GovData.CentralCheungChau
( fetch
) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.Bifunctor (first, second)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Csv (decode, decodeByName, FromRecord(..), FromNamedRecord(..), FromField(..), HasHeader(..), (.:), (.!))
import Data.Set (Set, singleton, isSubsetOf, empty, insert, intersection)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (DayOfWeek(..))
import Data.Time.Clock (NominalDiffTime)
import GHC.Exts (toList)
import Network.HTTP.Conduit (simpleHttp)
import Text.XML.Cursor (Cursor, fromDocument)


import Timetable hiding (timetables)
import Scraping.GovData.TimeString (parseTimeStr)

csv :: (MonadIO m, MonadCache m ByteString) => m ByteString
csv = withCache "GovData-CentralCheungChau-CSV" $
    simpleHttp "https://www.td.gov.hk/filemanager/en/content_1408/opendata/ferry_central_cc_timetable_eng.csv"


fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "GovData-CentralCheungChau" $ do
    res <- csv
    timetables <- case parseCsv res of
        Left err -> error err
        Right a -> pure a
    pure $ Route CentralCheungChau timetables

parseCsv :: ByteString -> Either String [Timetable NominalDiffTime]
parseCsv bs = toTimetables <$> (parseWithHeader bs <|> parse bs)

data EnumDays = MonToFri | Sat | SunAndHoliday

newtype Direction' = Direction' Direction
newtype Time' = Time' NominalDiffTime
newtype Days' = Days' (Set Day) deriving (Show)
newtype Remark' = Remark' (Entry -> (Set Day, Set Modifier))

data Entry = Entry
    { _direction :: Direction'
    , _days :: Days'
    , _time :: Time'
    , _remark :: Remark'
    }

instance FromRecord Entry where
    parseRecord v
        | length v == 4 = Entry <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
        | otherwise     = mzero

instance FromNamedRecord Entry where
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
        . emptyRemark
    -- 2 - denotes trip operated on Mondays to Fridays only (except public holidays)
    parseField "2" = pure $ Remark'
        $ addModifier FastFerry
        . modifyDays (intersection weekdays)
        . emptyRemark
    -- 3 - denotes Ordinary ferry service and freight service is allowed and trip operated on Saturdays only (except public holidays)
    parseField "3" = pure $ Remark'
        $ addModifier SlowFerry
        . addModifier Freight
        . modifyDays (const $ singleton $ Weekday Saturday)
        . emptyRemark
    -- 4 - denotes Ordinary ferry service and freight service is allowed and trip operated on Mondays to Fridays only (except public holidays)
    parseField "4" = pure $ Remark'
        $ addModifier SlowFerry
        . addModifier Freight
        . modifyDays (intersection weekdays)
        . emptyRemark
    parseField "" = pure $ Remark'
        $ addModifier FastFerry
        . emptyRemark
    parseField _ = fail "Cannot parse remark"


emptyRemark :: Entry -> (Set Day, Set Modifier)
emptyRemark (Entry _ (Days' d) _ _) = (d, empty)

addModifier :: Modifier -> (Set Day, Set Modifier) -> (Set Day, Set Modifier)
addModifier = second . insert

modifyDays :: (Set Day -> Set Day) -> (Set Day, Set Modifier) -> (Set Day, Set Modifier)
modifyDays = first

toTimetables :: [Entry] -> [Timetable NominalDiffTime]
toTimetables entries = do
    days <- [SunAndHoliday, Sat, MonToFri]
    direction <- [FromPrimary, ToPrimary]
    let ferries = toFerry <$> findEntries direction days
    [Timetable ferries (enumToDaySet days) direction]

    where
        remarkedEntries = patchDaySetByRemark <$> entries

        findEntries direction days = filter (match direction days) remarkedEntries
        match direction days (Entry (Direction' direction') (Days' days') _ _) = direction == direction' && (matchDays days days')
        matchDays days days' = (enumToDaySet days) `isSubsetOf` days'

        enumToDaySet SunAndHoliday = sunAndHoliday
        enumToDaySet Sat = singleton $ Weekday Saturday
        enumToDaySet MonToFri = weekdays

toFerry :: Entry -> Ferry NominalDiffTime
toFerry e@(Entry _ _ (Time' t) (Remark' r)) =
    Ferry t (snd $ r e)

patchDaySetByRemark :: Entry -> Entry
patchDaySetByRemark e@(Entry direction' _ time' (Remark' r)) = Entry direction' (Days' $ fst $ r e) time' (Remark' r)

parseWithHeader :: ByteString -> Either String [Entry]
parseWithHeader bs = (toList . snd) <$> decodeByName bs

parse :: ByteString -> Either String [Entry]
parse bs = toList <$> decode HasHeader bs
