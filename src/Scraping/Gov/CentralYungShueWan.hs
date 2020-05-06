module Scraping.Gov.CentralYungShueWan
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString)
import Data.Set (singleton)

import Text.XML.Cursor (Cursor, attributeIs, child, element, following,
                        ($.//), ($//), ($/), (>=>))
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack, isInfixOf)
import Data.Time.Clock (NominalDiffTime)
import Text.Regex.TDFA ((=~))
import Timetable hiding (timetables)
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility
import qualified Scraping.Gov as Gov (fetchCursor)


fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "CentralYungShueWan" $ do
    cursor <- Gov.fetchCursor
    pure $ Route CentralYungShueWan $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCentralYungShueWan cursor
    ct <- findTimetableCursors c
    cursorToTimetables ct

findCentralYungShueWan :: Cursor -> [Cursor]
findCentralYungShueWan cursor = cursor $.// (element "a") >=> attributeIs "name" "o04"

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
textHasDay day text = isInfixOf (pack(
    case day of
        Weekday  -> "Mondays"
        Saturday -> "Saturdays"
        Sunday   -> "Sundays"
        Holiday  -> "Sundays"
    )) text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Yung Shue Wan"
textForDirection FromPrimary   = pack "From Central"

tableToTimetables :: Day -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables day direction body =
    Timetable { ferries   = handleOverMidnight $ catMaybes $ map (toFerry (isDay day)) $ findDirection (textForDirection direction) body
              , day       = day
              , direction = direction
              }

findDirection :: Text -> [Text] -> [Text]
findDirection keyword list
    | isInfixOf keyword (list !! 0) = filter notEmpty $ tail $ pickOdd list
    | isInfixOf keyword (list !! 1) = filter notEmpty $ tail $ pickEven list
    | otherwise                     = error ("not found")


toFerry :: ([Char] -> Bool) -> Text -> Maybe (Ferry NominalDiffTime)
toFerry cond text = do
    (diffTime, modString) <- parseTimeStr text
    if cond modString
       then Just (Ferry { time = diffTime
                        , modifiers = mempty
                        })
       else Nothing

isDay :: Day -> [Char] -> Bool
isDay Sunday  _           = True
isDay Holiday _           = True
isDay Weekday  captures   = not $ elem '@' captures
isDay Saturday _          = True
