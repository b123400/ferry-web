module Scraping.Gov.CentralSokKwuWan
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString)
import Data.Set (singleton)
import Text.XML.Cursor (Cursor, attributeIs, element, following,
                        ($.//), ($//), ($/), (>=>))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text, pack, unpack, isInfixOf)
import Data.Time.Clock (NominalDiffTime)
import Text.Regex.TDFA ((=~))
import Timetable hiding (timetables)
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility

import qualified Scraping.Gov as Gov (fetchCursor)


fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "CentralSokKwuWan" $ do
    cursor <- Gov.fetchCursor
    pure $ Route CentralSokKwuWan $ timetables cursor

data Days = MonToSat | SunAndHoliday

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCentralSokKwuWan cursor
    days <- [SunAndHoliday, MonToSat]
    ct <- findTimetableCursors c
    direction <- [FromPrimary, ToPrimary]
    catMaybes $ return $ findTimetable days direction ct

findCentralSokKwuWan :: Cursor -> [Cursor]
findCentralSokKwuWan cursor = cursor $.// (element "a") >=> attributeIs "name" "o05"

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
        MonToSat      -> "Mondays"
        SunAndHoliday -> "Sundays"
    )) text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Sok Kwu Wan"
textForDirection FromPrimary   = pack "From Central"

tableToTimetables :: Days -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables days direction body =
    Timetable { ferries   = handleOverMidnight $ mapMaybe toFerry $ findDirection (textForDirection direction) body
              , days      = case days of MonToSat -> weekdaysAndSat
                                         SunAndHoliday -> sunAndHoliday
              , direction = direction
              }

findDirection :: Text -> [Text] -> [Text]
findDirection keyword list
    | isInfixOf keyword (list !! 0) = filter notEmpty $ tail $ pickOdd list
    | isInfixOf keyword (list !! 1) = filter notEmpty $ tail $ pickEven list
    | otherwise                     = error ("not found")

toFerry :: Text -> Maybe (Ferry NominalDiffTime)
toFerry text = do
    (diffTime, modString) <- parseTimeStr text
    Just (Ferry { time = diffTime
                , modifiers = mempty
                })
-- TODO '*' Additional sailing may be operated subject to passenger demand
