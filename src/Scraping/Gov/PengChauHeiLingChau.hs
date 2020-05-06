module Scraping.Gov.PengChauHeiLingChau
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text, pack, unpack, isInfixOf)
import Data.Time.Clock (NominalDiffTime)
import Text.Regex.TDFA ((=~))
import Text.XML.Cursor (Cursor, attributeIs, check, element, following,
                        ($.//), ($//), (>=>))
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility
import Timetable hiding (timetables)

import qualified Scraping.Gov as Gov (fetchCursor)


fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "PengChauHeiLingChau" $ do
    cursor <- Gov.fetchCursor
    pure $ Route PengChauHeiLingChau $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findPengChauHeiLingChau cursor
    ct <- findTimetableCursors c
    cursorToTimetables ct


findPengChauHeiLingChau :: Cursor -> [Cursor]
findPengChauHeiLingChau cursor = cursor $//(element "p") >=> (check (isInfixOf "Peng Chau - Hei Ling Chau" . flatContent))

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors = findTableElements . nthMatch 3 (matchName "table") . following

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
textHasDay day text = isInfixOf (pack (
    case day of
        Weekday  -> "Mondays"
        Saturday -> "Mondays"
        Sunday   -> "Sundays"
        Holiday  -> "Sundays"
    )) text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Hei Ling Chau"
textForDirection FromPrimary   = pack "From Peng Chau"

tableToTimetables :: Day -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables day direction body =
    Timetable { ferries   = handleOverMidnight $ mapMaybe toFerry $ findDirection (textForDirection direction) body
              , day       = day
              , direction = direction
              }

findDirection :: Text -> [Text] -> [Text]
findDirection keyword list
    | isInfixOf keyword (list !! 0) = filter notEmpty $ tail $ pickOdd list
    | isInfixOf keyword (list !! 1) = filter notEmpty $ tail $ pickEven list
    | otherwise                     = error ("not found" ++ (show keyword) ++ "/" ++ (show list))

toFerry :: Text -> Maybe (Ferry NominalDiffTime)
toFerry text = do
    (diffTime, modString) <- parseTimeStr text
    Just (Ferry { time = diffTime
                , modifiers = mempty
                })
