module Scraping.CoralSea.SamKaTsuenTungLungIsland where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Lazy (ByteString)
import Data.List (elemIndex)
import Data.Maybe (catMaybes)
import Data.Set (singleton)
import Data.String (IsString)
import Data.Text (isInfixOf)
import Data.Time.Calendar (DayOfWeek(Friday))
import Data.Time.Clock (NominalDiffTime)
import Text.XML.Cursor (Cursor, attributeIs, check, element, parent, followingSibling, ($.//), ($//), (>=>), (&.//))
import Scraping.Utility (flatContent, pickOdd, pickEven)
import Timetable hiding (timetables)

import Scraping.CoralSea.Timetable (fetchCursor)
import Scraping.CoralSea.TimeString (parseTimeStr)

fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime), MonadThrow m) => m (Route NominalDiffTime)
fetch = withCache "SamKaTsuenTungLungIsland" $ do
    cursor <- fetchCursor
    pure $ Route SamKaTsuenTungLungIsland $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    isWeekend <- [True, False]
    direction <- [FromPrimary, ToPrimary]
    timeForDayAndDirection cursor isWeekend direction

timeForDayAndDirection :: Cursor -> Bool -> Direction -> [Timetable NominalDiffTime]
timeForDayAndDirection cursor isWeekend direction = do
    let allSections = cursor $.// element "div" >=> attributeIs "class" "section-title"
        routeCursors = parent =<< filter ((==) "三家村 ⇋ 東龍島" . flatContent) allSections
        timetableElement = if isWeekend
            then routeCursors >>= ($// element "h4" &.// (check $ isInfixOf "星期六" . flatContent) &.// followingSibling &.// element "table")
            else routeCursors >>= ($// element "h4" &.// (check $ isInfixOf "星期五" . flatContent) &.// followingSibling &.// element "table")
        ths = flatContent <$> (timetableElement >>= ($// element "th"))
        times = flatContent <$> (timetableElement >>= ($// element "td"))
        parsedTime = fmap fst <$> parseTimeStr <$> times
        thisTimes = case elemIndex (titleForDirection isWeekend direction) ths of
            Just 0 -> catMaybes $ pickOdd parsedTime
            Just 1 -> catMaybes $ pickEven parsedTime
            _ -> []
        ferries = (\x -> Ferry x mempty) <$> thisTimes

    [Timetable ferries daySet direction]

    where daySet = if isWeekend then satSunAndHoliday else singleton $ Weekday Friday


titleForDirection :: IsString s => Bool -> Direction -> s
titleForDirection False FromPrimary = "去程"
titleForDirection False ToPrimary = "回程"
titleForDirection True FromPrimary = "由三家村出發"
titleForDirection True ToPrimary = "由東龍島出發"
