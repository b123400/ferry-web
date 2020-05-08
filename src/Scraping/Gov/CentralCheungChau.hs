module Scraping.Gov.CentralCheungChau
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Text.XML.Cursor (Cursor, attributeIs, element, following, ($.//), ($//), (>=>))
import Data.ByteString.Lazy (ByteString)
import Data.Set (singleton)
import Data.Text (Text, pack, unpack, isInfixOf)
import Data.Time.Calendar (DayOfWeek(Saturday))
import Timetable hiding (timetables)
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility
import Data.Maybe
import Data.Time.Clock (NominalDiffTime)

import qualified Scraping.Gov as Gov (fetchCursor)


fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "CentralCheungChau" $ do
    cursor <- Gov.fetchCursor
    pure $ Route CentralCheungChau $ timetables cursor

data Days = MonToFri | Sat | SunAndHoliday deriving (Show)

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCentralCheungChau cursor
    days <- [SunAndHoliday, Sat, MonToFri]
    ct <- findTimetableCursors c
    direction <- [FromPrimary, ToPrimary]
    catMaybes $ pure $ findTimetable days direction ct

findCentralCheungChau :: Cursor -> [Cursor]
findCentralCheungChau cursor = cursor $.// (element "a") >=> attributeIs "name" "o01"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    findTableElements . nthMatch 5 (matchName "table") . following

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
                MonToFri      -> isInfixOf (pack "Mondays") text
                Sat           -> isInfixOf (pack "Saturdays") text
                SunAndHoliday -> isInfixOf (pack "Sundays") text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Cheung Chau"
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
                        , modifiers = toModifiers modString
                        })
       else Nothing
    where toModifiers mods | elem '*' mods = singleton SlowFerry
                           | otherwise = singleton FastFerry


isDay :: Days -> [Char] -> Bool
isDay SunAndHoliday  _     = True
isDay MonToFri captures    = not $ elem '@' captures
isDay Sat captures         = not $ elem '#' captures
