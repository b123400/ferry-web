module Timetable.Metadata where

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
    }

data FareType
    = SlowFerryOrdinaryClass
    | SlowFerryDeluxeClass
    | FastFerry
    deriving (Eq, Ord, Show)

data Modifier
    = FromSecondaryOnly
    | DummyModifier -- Dummy modifier for proper json instance
    deriving (Eq, Ord, Show)

data Duration = Duration
    { ferryType :: Maybe T.Modifier
    , duration :: NominalDiffTime
    }

$(deriveJSON defaultOptions ''Metadata)
$(deriveJSON defaultOptions ''Fare)
$(deriveJSON defaultOptions ''Modifier)
$(deriveJSON defaultOptions ''Duration)
$(deriveJSON defaultOptions ''FareType)
