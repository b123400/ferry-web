module Render.Template.Ferry where

import Lucid
import Control.Monad (forM_)
import Data.Set (toAscList)
import Data.Time.Calendar (showGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Clock (NominalDiffTime)
import Render.Lang (Lang(..))
import Timetable (Timetable(..), Direction(..), Modifier(..), Ferry(..))

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


ferries_ :: DisplayTime t => Monad m => Lang -> [Ferry t] -> HtmlT m ()
ferries_ En [] = "No ferry within 24 hours"
ferries_ Hk [] = "24 小時內無船"
ferries_ l ferries = do
    ol_ $ do
        forM_ (zip [0..] ferries) $ \(index, f) -> do
            let mPrev = if index == 0 then Nothing else Just $ ferries !! (index - 1)
                today = localDay $ local $ time f
            case mPrev of
                Just prev | (localDay $ local $ time prev) /= today ->
                    div_ [class_ "date-separator"] (toHtml $ showGregorian today)
                _ -> pure ()
            ferry_ l f

ferry_ :: DisplayTimeOfDay t => Monad m => Lang -> Ferry t -> HtmlT m ()
ferry_ lang (Ferry time modifiers) = do
    li_ [class_ "timeslot"] $ do
        forM_ (toAscList modifiers) $ \modifier->
            div_ [class_ (ferryClassName modifier), alt_ (ferryAlt modifier)] ""
        time_ (toHtml $ timeString $ timeOfDay time)
        case diffString lang =<< diff time of
            Nothing -> pure ()
            Just diff -> span_ [class_ "reminder"] (toHtml diff)
    where
        ferryClassName modifier = case modifier of
            FastFerry -> "fast-ferry"
            SlowFerry -> "slow-ferry"
            OptionalFerry -> "optional-ferry"
            Freight -> "freight"

        ferryAlt modifier = case modifier of
            FastFerry -> "Fast Ferry"
            SlowFerry -> "Slow Ferry"
            OptionalFerry -> "Optional Ferry"
            Freight -> "Freight service is allowed"

        timeString (TimeOfDay hours minutes _) =
            (twoDigits hours) <> ":" <> (twoDigits minutes)

        diffString En d | d < 0 = Nothing
                        | d < 60 = Just "Now"
                        | d < 60*60 =
                           Just $ "In " <> (show $ minute d) <> " Minute" <> (if minute d == 1 then "" else "s")
                        | d < 60*60*2 =
                           Just $ "In " <> (show $ hour d) <> " Hour" <> (if hour d == 1 then "" else "s")
                        | otherwise = Nothing
        diffString Hk d | d < 0 = Nothing
                        | d < 60 = Just "現在"
                        | d < 60*60 =
                           Just $ "還有 " <> (show $ minute d) <> " 分鐘"
                        | d < 60*60*2 =
                           Just $ "還有 " <> (show $ hour d) <> " 小時"
                        | otherwise = Nothing
        minute :: NominalDiffTime -> Integer
        minute d = floor $ d/60.0
        hour :: NominalDiffTime -> Integer
        hour d = floor $ d/3600.0

        twoDigits num | num < 10 = "0" <> show num
                      | otherwise = show num
