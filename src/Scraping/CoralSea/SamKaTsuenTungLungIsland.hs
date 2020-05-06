module Scraping.CoralSea.SamKaTsuenTungLungIsland where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Lazy (ByteString)
import Data.List (elemIndex)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Time.Clock (NominalDiffTime)
import Text.Regex.TDFA ((=~))
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
    day <- [Weekday, Saturday, Sunday, Holiday]
    direction <- [FromPrimary, ToPrimary]
    timeForDayAndDirection cursor day direction

timeForDayAndDirection :: Cursor -> Day -> Direction -> [Timetable NominalDiffTime]
timeForDayAndDirection _ Weekday _ = []
timeForDayAndDirection cursor day direction = do
    let allSections = cursor $.// element "div" >=> attributeIs "class" "section-title"
        routeCursors = parent =<< filter ((==) "三家村 ⇋ 東龍島" . flatContent) allSections
        timetableElement = take 1 $ drop 1 $ routeCursors >>= ($// element "h4" &.// followingSibling &.// element "table")
        ths = flatContent <$> (timetableElement >>= ($// element "th"))
        times = flatContent <$> (timetableElement >>= ($// element "td"))
        parsedTime = fmap fst <$> parseTimeStr <$> times
        thisTimes = case elemIndex (titleForDirection direction) ths of
            Just 0 -> catMaybes $ pickOdd parsedTime
            Just 1 -> catMaybes $ pickEven parsedTime
            _ -> []
        ferries = (\x -> Ferry x mempty) <$> thisTimes

    [Timetable ferries day direction]


titleForDirection :: IsString s => Direction -> s
titleForDirection FromPrimary = "由三家村出發"
titleForDirection ToPrimary = "由東龍島出發"
