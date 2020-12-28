module Scraping.NWFF.NorthPointKowloonCity.Metadata () where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Csv (FromRecord(..), FromNamedRecord(..), FromField(..), decode, decodeByName, HasHeader(..), (.:), (.!))
import Data.Maybe (mapMaybe)
import Data.Set (Set, singleton, intersection)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (DayOfWeek(..))
import Data.Time.Clock (NominalDiffTime)
import GHC.Exts (toList)
import Network.HTTP.Conduit (simpleHttp)

import Timetable (Day(..), Island(..), everyday)
import qualified Timetable as T
import Timetable.Metadata (Metadata(..), Duration(..), FareType(..), Modifier(..), Fare(..))
import Timetable.Class (HasMetadata(..))

csv :: (MonadIO m, MonadCache m ByteString) => m ByteString
csv = withCache "NWFF-NorthPointKowloonCity-Metadata-CSV" $
    simpleHttp "https://www.td.gov.hk/filemanager/en/content_1408/opendata/ferry_np_klnc_faretable_eng.csv"

instance (MonadIO m, MonadCache m ByteString, MonadCache m Metadata) => HasMetadata m NorthPointKowloonCity where
    fetchMetadata _ = withCache "NWFF-NorthPointKowloonCity-Metadata" $ do
        res <- csv
        metadata <- case parseCsv res of
            Left err -> error err
            Right a -> pure a
        pure metadata

newtype Passenger' = Passenger' String
newtype Days' = Days' (Set Day) deriving (Show)
newtype Modifier' = Modifier' (Set Modifier)
newtype Fare' = Fare' Double

data Entry = Entry Passenger' Days' Fare' Modifier'

instance FromRecord Entry where
    parseRecord v
        | length v == 4 = Entry <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
        | otherwise     = mzero

instance FromNamedRecord Entry where
    parseNamedRecord m = Entry <$> m .: "Passenger"
                               <*> m .: "Service Date"
                               <*> m .: "Fare (HK$)"
                               <*> m .: "Remark"

instance FromField Passenger' where
    parseField = pure . Passenger' . unpack . decodeUtf8

instance FromField Days' where
    parseField "Daily" = pure $ Days' everyday
    parseField _ = fail "Cannot parse Days"

instance FromField Fare' where
    parseField = pure . Fare' . read . handleFree . removeBadCharacters . unpack . decodeUtf8
        where removeBadCharacters = filter (\x -> x /= '$' && x /= ' ')
              handleFree "Free" = "0"
              handleFree x = x

instance FromField Modifier' where
    parseField _ = pure $ Modifier' mempty

parseCsv :: ByteString -> Either String Metadata
parseCsv bs = toMetadata <$> (parseWithHeader bs <|> parse bs)

parseWithHeader :: ByteString -> Either String [Entry]
parseWithHeader bs = (toList . snd) <$> decodeByName bs

parse :: ByteString -> Either String [Entry]
parse bs = toList <$> decode HasHeader bs

toMetadata :: [Entry] -> Metadata
toMetadata entries = Metadata { fares = fares, durations = durations }
    where
        fares = entryToFare <$> entries
        entryToFare (Entry (Passenger' passenger') (Days' daySet) (Fare' fare) (Modifier' modifiers)) = Fare
            { passenger = passenger'
            , days = daySet
            , fare = fare
            , type_ = SlowFerryOrdinaryClass
            , modifiers = modifiers
            }

        durations =
            [ Duration Nothing (14 * 60)
            ]
