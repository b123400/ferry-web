module Scraping.Gov.AberdeenSokKwuWan
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Text.XML.Cursor (Cursor, attributeIs, element, following,
                        ($.//), ($//), (>=>))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, unpack, isInfixOf)
import Text.Regex.TDFA ((=~))
import Timetable hiding (timetables)
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility
import Data.Maybe
import Data.Time.Clock (NominalDiffTime)

import qualified Scraping.Gov as Gov (fetchCursor)

fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "AberdeenSokKwuWan" $ do
    cursor <- Gov.fetchCursor
    pure $ Route AberdeenSokKwuWan $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findAberdeenSokKwuWan cursor
    ct <- findTimetableCursors c
    cursorToTimetables ct

findAberdeenSokKwuWan :: Cursor -> [Cursor]
findAberdeenSokKwuWan cursor = cursor $.// (element "a") >=> attributeIs "name" "o10"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    findTableElements . nthMatch 3 (matchName "table") . following

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
    | otherwise =
        let trs = timeTable $// (element "tr")
        in Just $ tableToTimetables day direction (flatContent <$> (getTDs =<< filter hasTwoTd trs))

hasDay :: Cursor -> Day -> Bool
hasDay cursor day =
    case cursor $// (element "td") of
        (x:_) -> textHasDay day $ flatContent x
        _ -> False
    where
        textHasDay :: Day -> Text -> Bool
        textHasDay day text =
            case day of
                Weekday          -> isInfixOf (pack "Mondays") text
                Saturday         -> isInfixOf (pack "Saturdays") text
                Sunday           -> isInfixOf (pack "Sundays") text
                Holiday          -> isInfixOf (pack "holidays") text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Sok Kwu Wan"
textForDirection FromPrimary   = pack "From Aberdeen"

tableToTimetables :: Day -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables day direction body =
    Timetable { ferries   = handleOverMidnight $ mapMaybe toFerry $ findDirection (textForDirection direction) body
              , day       = day
              , direction = direction
              }

findDirection :: Text -> [Text] -> [Text]
findDirection keyword list
    | isInfixOf keyword (list !! 0) = filter notEmpty $ tail $ pickOdd list
    | isInfixOf keyword (list !! 1) = filter notEmpty $ tail $ pickEven list
    | otherwise                     = error ("not found")

toFerry :: Text -> Maybe (Ferry NominalDiffTime)
toFerry text = do
    (diffTime, _) <- parseTimeStr text
    Just (Ferry { time = diffTime
                , modifiers = mempty
                })
