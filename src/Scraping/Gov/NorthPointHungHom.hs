module Scraping.Gov.NorthPointHungHom
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
fetch = withCache "NorthPointHungHom" $ do
    cursor <- Gov.fetchCursor
    pure $ Route NorthPointHungHom $ timetables cursor

data Days = MonToFri | SatSunAndHoliday deriving (Show)

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findNorthPointHungHom cursor
    days <- [SatSunAndHoliday, MonToFri]
    let ct = findTimetableCursor c
    direction <- [FromPrimary, ToPrimary]
    catMaybes $ pure $ findTimetable days direction ct

findNorthPointHungHom :: Cursor -> [Cursor]
findNorthPointHungHom cursor = cursor $.// (element "a") >=> attributeIs "name" "i01"

findTimetableCursor :: Cursor -> Cursor
findTimetableCursor = nthMatch 4 (matchName "table") . following

findTimetable :: Days -> Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable days direction timeTable =
    let trs = timeTable $// (element "tr")
    in Just $ tableToTimetables days direction (flatContent <$> (getTDs =<< filter hasTwoTd trs))

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Hung Hom to North Point"
textForDirection FromPrimary   = pack "From North Point to Hung Hom"

tableToTimetables :: Days -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables days direction body =
    Timetable { ferries   = handleOverMidnight $ catMaybes $ map (toFerry (isDay days)) $ findDirection (textForDirection direction) body
              , days      = case days of MonToFri -> weekdays
                                         -- Sat -> singleton $ Weekday Saturday
                                         SatSunAndHoliday -> satSunAndHoliday
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
       then Just (Ferry { time = fixStupidIssue diffTime
                        , modifiers = toModifiers modString
                        })
       else Nothing
    where toModifiers mods | elem '*' mods = singleton SlowFerry
                           | otherwise = singleton FastFerry

-- The timetable mistyped 11:35 am to 11:35 pm
fixStupidIssue :: NominalDiffTime -> NominalDiffTime
fixStupidIssue diffTime = if diffTime == 84900 then 41700 else diffTime

isDay :: Days -> [Char] -> Bool
isDay SatSunAndHoliday captures = not $ elem '*' captures
isDay MonToFri _ = True
