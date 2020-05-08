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
import Data.Time.Calendar (DayOfWeek(Saturday))
import Data.Time.Clock (NominalDiffTime)
import Timetable hiding (timetables)
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility
import qualified Scraping.Gov as Gov (fetchCursor)


fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "CentralYungShueWan" $ do
    cursor <- Gov.fetchCursor
    pure $ Route CentralYungShueWan $ timetables cursor

data Days = MonToFri | Sat | SunAndHoliday

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCentralYungShueWan cursor
    days <- [SunAndHoliday, Sat, MonToFri]
    ct <- findTimetableCursors c
    direction <- [FromPrimary, ToPrimary]
    catMaybes $ return $ findTimetable days direction ct

findCentralYungShueWan :: Cursor -> [Cursor]
findCentralYungShueWan cursor = cursor $.// (element "a") >=> attributeIs "name" "o04"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors = findTableElements . nthMatch 3 (matchName "table") . following

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
textHasDay days text = isInfixOf (pack(
    case days of
        MonToFri      -> "Mondays"
        Sat           -> "Saturdays"
        SunAndHoliday -> "Sundays"
    )) text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Yung Shue Wan"
textForDirection FromPrimary   = pack "From Central"

tableToTimetables :: Days -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables days direction body =
    Timetable { ferries   = handleOverMidnight $ catMaybes $ map (toFerry (isDay days)) $ findDirection (textForDirection direction) body
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


toFerry :: ([Char] -> Bool) -> Text -> Maybe (Ferry NominalDiffTime)
toFerry cond text = do
    (diffTime, modString) <- parseTimeStr text
    if cond modString
       then Just (Ferry { time = diffTime
                        , modifiers = mempty
                        })
       else Nothing

isDay :: Days -> [Char] -> Bool
isDay SunAndHoliday  _    = True
isDay MonToFri  captures  = not $ elem '@' captures
isDay Sat _               = True
