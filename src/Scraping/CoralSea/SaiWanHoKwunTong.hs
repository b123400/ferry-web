module Scraping.CoralSea.SaiWanHoKwunTong where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Time.Clock (NominalDiffTime)
import Text.Regex.TDFA ((=~))
import Text.XML.Cursor (Cursor, attributeIs, check, element, parent, followingSibling, ($.//), ($//), (>=>), (&.//))
import Scraping.Utility (flatContent)
import Timetable hiding (timetables)

import Scraping.CoralSea.Timetable (fetchCursor)
import Scraping.CoralSea.TimeString (parseTimeStr)

fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime), MonadThrow m) => m (Route NominalDiffTime)
fetch = withCache "SaiWanHoKwunTong" $ do
    cursor <- fetchCursor
    pure $ Route SaiWanHoKwunTong $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    day <- [Weekday, Saturday, Sunday, Holiday]
    direction <- [ToIsland, FromIsland]
    timeForDayAndDirection cursor day direction

timeForDayAndDirection :: Cursor -> Day -> Direction -> [Timetable NominalDiffTime]
timeForDayAndDirection cursor day direction = do
    let allSections = cursor $.// element "div" >=> attributeIs "class" "section-title"
        routeCursors = parent =<< filter ((==) "西灣河 ⇋ 觀塘" . flatContent) allSections
        timetableElement = take 1 $ routeCursors >>= ($// element "h4"
                                                      &.// (check $ (==) (titleForDirection direction) . flatContent)
                                                      &.// followingSibling
                                                      &.// element "table")
        times = flatContent <$> (timetableElement >>= ($// element "td"))
        parsedTime = map fst $ filter isToday $ mapMaybe parseTimeStr times
        ferries = (\x -> Ferry x SlowFerry) <$> parsedTime

    [Timetable ferries day direction]

    where isToday (_, weekDayOnly) = day == Weekday || not weekDayOnly


titleForDirection :: IsString s => Direction -> s
titleForDirection ToIsland = "由西灣河出發"
titleForDirection FromIsland = "由觀塘出發"