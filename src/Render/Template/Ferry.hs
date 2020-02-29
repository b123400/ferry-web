module Render.Template.Ferry where

import Lucid
import Control.Monad (forM_)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Time.Clock (NominalDiffTime)
import Timetable (Timetable(..), Direction(..), FerryType(..), Ferry(..))

class DisplayTime t where
    local :: t -> LocalTime
    diff :: t -> Maybe NominalDiffTime

instance DisplayTime LocalTime where
    local = id
    diff _ = Nothing

instance DisplayTime (LocalTime, NominalDiffTime) where
    local = fst
    diff = Just . snd

ferries_ :: DisplayTime t => Monad m => [Ferry t] -> HtmlT m ()
ferries_ ferries = do
    ol_ $ do
        forM_ ferries ferry_
        li_ [class_ "more"] "More..."

ferry_ :: DisplayTime t => Monad m => Ferry t -> HtmlT m ()
ferry_ (Ferry time ferryType) = do
    li_ [class_ "timeslot"] $ do
        div_ [class_ (ferryClassName ferryType), alt_ (ferryAlt ferryType)] ""
        time_ (toHtml $ timeString $ local time)
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

        timeString (LocalTime _ (TimeOfDay hours minutes _)) =
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
