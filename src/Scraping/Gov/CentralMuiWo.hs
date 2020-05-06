module Scraping.Gov.CentralMuiWo
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString)
import Data.Set (singleton)
import Data.Text (Text, pack, unpack, isInfixOf)
import Text.Regex.TDFA ((=~))
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

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCentralMuiWo cursor
    ct <- findTimetableCursors c
    cursorToTimetables ct

findCentralMuiWo :: Cursor -> [Cursor]
findCentralMuiWo cursor = cursor $.// (element "a") >=> attributeIs "name" "o02"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    findTableElements . nthMatch 3 (matchName "table") . following


findTableElements :: Cursor -> [Cursor]
findTableElements c = c $// (element "table")

cursorToTimetables :: Cursor -> [Timetable NominalDiffTime]
cursorToTimetables timeTable = catMaybes $ do
    day <- [Weekday, Saturday, Sunday, Holiday]
    direction <- [FromPrimary, ToPrimary]
    return $ findTimetable day direction timeTable

findTimetable :: Day -> Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable day direction timeTable
    | not $ hasDay timeTable day = Nothing
    | otherwise                  =
        let trs = timeTable $// (element "tr")
        in Just $ tableToTimetables day direction $ flatContent <$> (getTDs =<< filter hasTwoTd trs)

hasDay :: Cursor -> Day -> Bool
hasDay cursor day = textHasDay day $ flatContent $ head $ cursor $// (element "td")

textHasDay :: Day -> Text -> Bool
textHasDay Weekday  text = isInfixOf (pack "Mondays")   text
textHasDay Saturday text = isInfixOf (pack "Saturdays") text
textHasDay Sunday  text = isInfixOf (pack "Sundays")   text
textHasDay Holiday  text = isInfixOf (pack "Sundays")   text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "Mui Wo"
textForDirection FromPrimary   = pack "From Central"

tableToTimetables :: Day -> Direction -> [Text] -> (Timetable NominalDiffTime)
tableToTimetables day direction body =
    Timetable { ferries   = handleOverMidnight $ mapMaybe toFerry $ findDirection (textForDirection direction) body
              , day       = day
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
    where toModifiers mods | elem '*' mods = singleton SlowFerry
                           | otherwise = singleton FastFerry
