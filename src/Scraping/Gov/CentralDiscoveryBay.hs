module Scraping.Gov.CentralDiscoveryBay
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Text.XML.Cursor (Cursor, attributeIs, element, following,
                        ($.//), ($//), (>=>))
import Data.ByteString.Lazy (ByteString)
import Data.Set (singleton)
import Data.Text (Text, pack, unpack, isInfixOf)
import Data.Time.Calendar (DayOfWeek(Saturday, Sunday))
import Timetable hiding (timetables)
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility
import Data.Maybe
import Data.Time.Clock (NominalDiffTime)

import qualified Scraping.Gov as Gov (fetchCursor)

fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "CentralDiscoveryBay" $ do
    cursor <- Gov.fetchCursor
    pure $ Route CentralDiscoveryBay $ timetables cursor

data Days = MonToFri | Sat | Sun | Holidays

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCentralDiscoveryBay cursor
    days <- [Sun, Holidays, Sat, MonToFri]
    ct <- findTimetableCursors c
    direction <- [FromPrimary, ToPrimary]
    catMaybes $ return $ findTimetable days direction ct

findCentralDiscoveryBay :: Cursor -> [Cursor]
findCentralDiscoveryBay cursor = cursor $.// (element "a") >=> attributeIs "name" "o11"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    findTableElements . nthMatch 6 (matchName "table") . following

findTableElements :: Cursor -> [Cursor]
findTableElements c = c $// (element "table")

findTimetable :: Days -> Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable days direction timeTable
    | not $ hasDay timeTable days = Nothing
    | otherwise =
        let trs = timeTable $// (element "tr")
        in Just $ tableToTimetables days direction (flatContent <$> (getTDs =<< filter hasTwoTd trs))

hasDay :: Cursor -> Days -> Bool
hasDay cursor days =
    case cursor $// (element "td") of
        (x:_) -> textHasDay $ flatContent x
        _ -> False
    where
        textHasDay :: Text -> Bool
        textHasDay text =
            case days of
                MonToFri    -> isInfixOf (pack "Mondays") text
                Sat         -> isInfixOf (pack "Saturdays") text
                Sun         -> isInfixOf (pack "Sundays") text
                Holidays    -> isInfixOf (pack "Sundays") text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Discovery Bay"
textForDirection FromPrimary   = pack "From Central"

tableToTimetables :: Days -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables days direction body =
    Timetable { ferries   = handleOverMidnight $ catMaybes $ map (toFerry (isDay days)) $ findDirection (textForDirection direction) body
              , days      = case days of MonToFri -> weekdays
                                         Sat -> singleton $ Weekday Saturday
                                         Sun -> singleton $ Weekday Sunday
                                         Holidays  -> singleton $ Holiday
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
-- isViaDisneyland  = elem '*' mods
-- The sailing will operate via Disneyland Resort Pier (Due to repair work in progress, the stopping point at Disneyland Resort Pier is temporary suspended.)

isDay :: Days -> [Char] -> Bool
isDay Sun  _            = True
isDay Holidays captures = not $ elem '#' captures
isDay MonToFri _        = True
isDay Sat _             = True
