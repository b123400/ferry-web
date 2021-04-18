module Scraping.Gov.CentralMuiWo
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString)
import Data.Set (singleton)
import Data.Text (Text, pack, unpack, isInfixOf)
import Data.Time.Calendar (DayOfWeek(Saturday))
import Data.Time.Clock (NominalDiffTime)
import Data.Maybe (catMaybes, mapMaybe)
import Text.XML.Cursor (Cursor, attributeIs, element, following,
                        ($.//), ($//), (>=>))
import Timetable hiding (timetables)
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility

import qualified Scraping.Gov as Gov (fetchCursor)

fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "CentralMuiWo" $ do
    cursor <- Gov.fetchCursor
    pure $ Route CentralMuiWo $ timetables cursor

data Days = MonToFri | Sat | SunAndHoliday

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCentralMuiWo cursor
    days <- [SunAndHoliday, Sat, MonToFri]
    ct <- findTimetableCursors c
    direction <- [FromPrimary, ToPrimary]
    catMaybes $ return $ findTimetable days direction ct

findCentralMuiWo :: Cursor -> [Cursor]
findCentralMuiWo cursor = cursor $.// (element "a") >=> attributeIs "name" "o02"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    findTableElements . nthMatch 3 (matchName "table") . following


findTableElements :: Cursor -> [Cursor]
findTableElements c = c $// (element "table")

findTimetable :: Days -> Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable days direction timeTable
    | not $ hasDay timeTable days = Nothing
    | otherwise                  =
        let trs = timeTable $// (element "tr")
        in Just $ tableToTimetables days direction $ flatContent <$> (getTDs =<< filter hasTwoTd trs)

hasDay :: Cursor -> Days -> Bool
hasDay cursor days = textHasDay days $ flatContent $ head $ cursor $// (element "td")

textHasDay :: Days -> Text -> Bool
textHasDay MonToFri text = isInfixOf (pack "Mondays") text
textHasDay Sat text = isInfixOf (pack "Saturdays") text
textHasDay SunAndHoliday  text = isInfixOf (pack "Sundays") text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "Mui Wo"
textForDirection FromPrimary   = pack "From Central"

tableToTimetables :: Days -> Direction -> [Text] -> (Timetable NominalDiffTime)
tableToTimetables days direction body =
    Timetable { ferries   = handleOverMidnight $ mapMaybe toFerry $ findDirection (textForDirection direction) body
              , days      = case days of MonToFri -> weekdays
                                         Sat -> singleton $ Weekday Saturday
                                         SunAndHoliday -> sunAndHoliday
              , direction = direction
              }

findDirection :: Text -> [Text] -> [Text]
findDirection keyword list
    | isInfixOf keyword (list !! 0) = filter notEmpty $ tail $ pickOdd list
    | isInfixOf keyword (list !! 1) = filter notEmpty $ tail $ pickEven list
    | otherwise                     = error ("not found")


toFerry :: Text -> Maybe (Ferry NominalDiffTime)
toFerry text = do
    (diffTime, modString) <- parseTimeStr text
    Just (Ferry { time = diffTime
                , modifiers = toModifiers modString
                })
    where toModifiers mods | elem '@' mods = singleton FastFerry
                           | elem '*' mods = singleton SlowFerry
                           | otherwise = singleton FastFerry
