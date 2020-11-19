module Scraping.GovData.CentralPengChau.Metadata () where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString, fromStrict, isPrefixOf, intercalate)
import Data.ByteString.Lazy.Char8 (split)
import Data.Csv (FromRecord(..), FromNamedRecord(..), FromField(..), decode, decodeByName, HasHeader(..), (.:), (.!))
import Data.Maybe (mapMaybe)
import Data.Set (Set, singleton, intersection)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (DayOfWeek(..))
import Data.Time.Clock (NominalDiffTime)
import GHC.Exts (toList)
import Network.HTTP.Conduit (simpleHttp)

import Timetable (Day(..), Island(..), sunAndHoliday, weekdaysAndSat, everyday)
import qualified Timetable as T
import Timetable.Metadata (Metadata(..), Duration(..), FareType(..), Modifier(..), Fare(..))
import Timetable.Class (HasMetadata(..))
import Scraping.GovData.Csv (EnumDays(..))
import Scraping.GovData.TimeString (parseTimeStr)

csv :: (MonadIO m, MonadCache m ByteString) => m ByteString
csv = withCache "GovData-CentralPengChau-Metadata-CSV" $
    simpleHttp "https://www.td.gov.hk/filemanager/en/content_1408/opendata/ferry_central_pc_faretable_eng.csv"

instance (MonadIO m, MonadCache m ByteString, MonadCache m Metadata) => HasMetadata m CentralPengChau where
    fetchMetadata _ = withCache "GovData-CentralPengChau-Metadata" $ do
        res <- csv
        metadata <- case parseCsv $ patchCsv res of
            Left err -> error err
            Right a -> pure a
        pure metadata

-- There is a line of ",,,.,," in the csv
-- probably due to human error, we need to filter it out
patchCsv :: ByteString -> ByteString
patchCsv = intercalate "\n" . filter (not . isPrefixOf ",") . split '\n'

newtype Route' = Route' String
newtype Passenger' = Passenger' String
newtype Days' = Days' (Set Day) deriving (Show)
newtype Modifier' = Modifier' (Set Modifier)
newtype Fare' = Fare' Double
newtype FareType' = FareType' FareType

data Entry = Entry Route' Passenger' Days' FareType' Fare' Modifier'

instance FromRecord Entry where
    parseRecord v
        | length v == 6 = Entry <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5
        | otherwise     = mzero

instance FromNamedRecord Entry where
    parseNamedRecord m = Entry <$> m .: "Route"
                               <*> m .: "Passenger"
                               <*> m .: "Service Date"
                               <*> m .: "Ferry Type"
                               <*> m .: "Fare (HK$)"
                               <*> m .: "Remark"

instance FromField Route' where
    parseField = pure . Route' . unpack . decodeUtf8

instance FromField Passenger' where
    parseField = pure . Passenger' . unpack . decodeUtf8

instance FromField Days' where
    parseField "Mondays to Saturdays except public holidays" = pure $ Days' weekdaysAndSat
    parseField "Sundays and public holidays" = pure $ Days' sunAndHoliday
    parseField "Daily" = pure $ Days' everyday
    parseField _ = fail "Cannot parse Days"

instance FromField Fare' where
    parseField = pure . Fare' . read . unpack . decodeUtf8

instance FromField FareType' where
    parseField "Ordinary Ferry Service" = pure $ FareType' SlowFerryOrdinaryClass
    parseField "Fast Ferry Service" = pure $ FareType' FastFerry
    parseField "-" = pure $ FareType' SlowFerryOrdinaryClass
    parseField _ = fail "Cannot parse modifier"

instance FromField Modifier' where
    parseField "1" = pure $ Modifier' $ singleton FromSecondaryOnly
    parseField "" = pure $ Modifier' mempty
    parseField _ = fail "Cannot parse remark"

parseCsv :: ByteString -> Either String Metadata
parseCsv bs = toMetadata <$> (parseWithHeader bs <|> parse bs)

parseWithHeader :: ByteString -> Either String [Entry]
parseWithHeader bs = (toList . snd) <$> decodeByName bs

parse :: ByteString -> Either String [Entry]
parse bs = toList <$> decode HasHeader bs

toMetadata :: [Entry] -> Metadata
toMetadata entries = Metadata { fares = fares, durations = durations }
    where
        fares = mapMaybe entryToFare entries
        entryToFare (Entry (Route' "Central - Peng Chau") (Passenger' passenger') (Days' daySet) (FareType' fareType) (Fare' fare) (Modifier' modifiers)) =
            Just $ Fare
                { passenger = passenger'
                , days = daySet
                , fare = fare
                , type_ = fareType
                , modifiers = modifiers
                }
        entryToFare _ = Nothing

        durations =
            [ Duration (Just T.FastFerry) (30 * 60)
            , Duration Nothing (40 * 60)
            ]
