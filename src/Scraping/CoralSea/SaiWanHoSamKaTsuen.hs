module Scraping.CoralSea.SaiWanHoSamKaTsuen where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Time.Clock (NominalDiffTime)
import Text.XML.Cursor (Cursor, attributeIs, check, element, parent, followingSibling, ($.//), ($//), (>=>), (&.//))
import Scraping.Utility (flatContent)
import Timetable hiding (timetables)
import Timetable.Class (HasTimetable(..))

import Scraping.CoralSea.Timetable (fetchCursor)
import Scraping.CoralSea.TimeString (parseTimeStr)

instance (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime), MonadThrow m) => HasTimetable m SaiWanHoSamKaTsuen where
  fetchTimetable _ = withCache "SaiWanHoSamKaTsuen" $ do
      cursor <- fetchCursor
      pure $ Route SaiWanHoSamKaTsuen $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    direction <- [FromPrimary, ToPrimary]
    timeForDayAndDirection cursor direction

timeForDayAndDirection :: Cursor -> Direction -> [Timetable NominalDiffTime]
timeForDayAndDirection cursor direction = do
    let allSections = cursor $.// element "div" >=> attributeIs "class" "section-title"
        routeCursors = parent =<< filter ((==) "西灣河 ⇋ 三家村" . flatContent) allSections
        timetableElement = take 1 $ routeCursors >>= ($// element "h4"
                                                      &.// (check $ (==) (titleForDirection direction) . flatContent)
                                                      &.// followingSibling
                                                      &.// element "table")
        times = flatContent <$> (timetableElement >>= ($// element "td"))
        parsedTime = fst <$> mapMaybe parseTimeStr times
        ferries = (\x -> Ferry x mempty) <$> parsedTime

    [Timetable ferries everyday direction]


titleForDirection :: IsString s => Direction -> s
titleForDirection FromPrimary = "由西灣河出發"
titleForDirection ToPrimary = "由三家村出發"
