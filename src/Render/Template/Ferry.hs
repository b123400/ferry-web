module Render.Template.Ferry where

import Lucid
import Control.Monad (forM_)
import Data.Time.Calendar (showGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Clock (NominalDiffTime)
import Timetable (Timetable(..), Direction(..), FerryType(..), Ferry(..))

class DisplayTimeOfDay t => DisplayTime t where
    local :: t -> LocalTime

instance DisplayTime LocalTime where
    local = id

instance DisplayTime (LocalTime, NominalDiffTime) where
    local = fst

class DisplayTimeOfDay t where
    timeOfDay :: t -> TimeOfDay
    diff :: t -> Maybe NominalDiffTime

instance DisplayTimeOfDay TimeOfDay where
    timeOfDay = id
    diff _ = Nothing

instance DisplayTimeOfDay LocalTime where
    timeOfDay = localTimeOfDay . local
    diff _ = Nothing

instance DisplayTimeOfDay (LocalTime, NominalDiffTime) where
    timeOfDay = localTimeOfDay . local
    diff = Just . snd


ferries_ :: DisplayTime t => Monad m => [Ferry t] -> HtmlT m ()
ferries_ [] = toHtml ("No ferry within 24 hours" :: String)
ferries_ ferries = do
    ol_ $ do
        forM_ (zip [0..] ferries) $ \(index, f) -> do
            let mPrev = if index == 0 then Nothing else Just $ ferries !! (index - 1)
                today = localDay $ local $ time f
            case mPrev of
                Just prev | (localDay $ local $ time prev) /= today ->
                    div_ [class_ "date-separator"] (toHtml $ showGregorian today)
                _ -> pure ()
            ferry_ f

ferry_ :: DisplayTimeOfDay t => Monad m => Ferry t -> HtmlT m ()
ferry_ (Ferry time ferryType) = do
    li_ [class_ "timeslot"] $ do
        div_ [class_ (ferryClassName ferryType), alt_ (ferryAlt ferryType)] ""
        time_ (toHtml $ timeString $ timeOfDay time)
        case diffString =<< diff time of
            Nothing -> pure ()
            Just diff -> span_ [class_ "reminder"] (toHtml diff)
    where
        ferryClassName ferryType = case ferryType of
            FastFerry -> "fast-ferry"
            SlowFerry -> "slow-ferry"
            OptionalFerry -> "optional-ferry"

        ferryAlt ferryType = case ferryType of
            FastFerry -> "Fast Ferry"
            SlowFerry -> "Slow Ferry"
            OptionalFerry -> "Optional Ferry"

        timeString (TimeOfDay hours minutes _) =
            (twoDigits hours) <> ":" <> (twoDigits minutes)

        diffString d | d < 0 = Nothing
                     | d < 60 = Just "Now"
                     | d < 60*60 =
                        Just $ "In " <> (show minute) <> " Minute" <> (if minute == 1 then "" else "s")
                     | d < 60*60*2 =
                        Just $ "In " <> (show hour) <> " Hour" <> (if hour == 1 then "" else "s")
                     | otherwise = Nothing
            where minute :: Integer
                  minute = floor $ d/60.0
                  hour :: Integer
                  hour = floor $ d/3600.0

        twoDigits num | num < 10 = "0" <> show num
                      | otherwise = show num
