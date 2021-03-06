module Timetable.Metadata where

import Data.Aeson (ToJSON(..), FromJSON(..), ToJSONKey(..), Value(String), (.=), (.:), object, withObject)
import Data.Aeson.TH
import Data.Set (Set)
import Data.Time.Clock (NominalDiffTime)
import qualified Timetable as T

data Metadata = Metadata
    { fares :: [Fare]
    , durations :: [Duration]
    }

data Fare = Fare
    { passenger :: String
    , days :: Set T.Day
    , fare :: Double
    , type_ :: FareType
    , modifiers :: Set Modifier
    } deriving (Eq, Ord, Show)

data FareType
    = SlowFerryOrdinaryClass
    | SlowFerryDeluxeClass
    | FastFerry
    deriving (Eq, Ord, Show)

data Modifier
    = FromSecondaryOnly
    | RegisteredUser -- e.g. Ma Wan - Tsuen Wan. Users registered with Park Island Transport Company Ltd
    deriving (Eq, Ord, Show)

data Duration = Duration
    { ferryType :: Maybe T.Modifier
    , duration :: NominalDiffTime
    } deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Metadata)

instance ToJSON Fare where
    toJSON (Fare passenger days fare type_ modifiers) = object
        [ "passenger" .= passenger
        , "days" .= days
        , "fare" .= fare
        , "type" .= type_
        , "modifiers" .=modifiers
        ]

instance FromJSON Fare where
    parseJSON = withObject "Fare" $ \o -> do
        passenger <- o .: "passenger"
        days <- o .: "days"
        fare <- o .: "fare"
        type_ <- o .: "type"
        modifier <- o .: "modifier"
        pure $ Fare passenger days fare type_ modifier

$(deriveJSON defaultOptions ''Modifier)
$(deriveJSON defaultOptions ''Duration)
$(deriveJSON defaultOptions ''FareType)

ferryTypeToTimetableModifier :: FareType -> T.Modifier
ferryTypeToTimetableModifier SlowFerryOrdinaryClass = T.SlowFerry
ferryTypeToTimetableModifier SlowFerryDeluxeClass = T.SlowFerry
ferryTypeToTimetableModifier FastFerry = T.FastFerry
