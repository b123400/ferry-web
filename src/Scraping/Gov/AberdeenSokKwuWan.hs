module Scraping.Gov.AberdeenSokKwuWan
(
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Text.XML.Cursor (Cursor, attributeIs, element, following,
                        ($.//), ($//), (>=>))
import Data.ByteString.Lazy (ByteString)
import Data.Set (Set)
import Data.Text (Text, pack, unpack, isInfixOf)
import Timetable hiding (timetables)
import Timetable.Class (HasTimetable(..))
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility
import Data.Maybe
import Data.Time.Clock (NominalDiffTime)

import qualified Scraping.Gov as Gov (fetchCursor)

-- instance (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => HasTimetable m AberdeenSokKwuWan where
--     fetchTimetable _ = withCache "AberdeenSokKwuWan" $ do
--         cursor <- Gov.fetchCursor
--         pure $ Route AberdeenSokKwuWan $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findAberdeenSokKwuWan cursor
    isWeekday <- [False, True]
    ct <- findTimetableCursors c
    direction <- [FromPrimary, ToPrimary]
    catMaybes $ return $ findTimetable isWeekday direction ct

findAberdeenSokKwuWan :: Cursor -> [Cursor]
findAberdeenSokKwuWan cursor = cursor $.// (element "a") >=> attributeIs "name" "o10"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    findTableElements . nthMatch 3 (matchName "table") . following

findTableElements :: Cursor -> [Cursor]
findTableElements c = c $// (element "table")

findTimetable :: Bool -> Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable isWeekday direction timeTable
    | not $ hasDay timeTable isWeekday = Nothing
    | otherwise =
        let trs = timeTable $// (element "tr")
        in Just $ tableToTimetables daySet direction (flatContent <$> (getTDs =<< filter hasTwoTd trs))
    where daySet = if isWeekday then weekdaysAndSat else sunAndHoliday

hasDay :: Cursor -> Bool -> Bool
hasDay cursor isWeekday =
    case cursor $// (element "td") of
        (x:_) -> textHasDay $ flatContent x
        _ -> False
    where
        textHasDay :: Text -> Bool
        textHasDay = if isWeekday
                     then isInfixOf (pack "Mondays")
                     else isInfixOf (pack "Sundays")

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Sok Kwu Wan"
textForDirection FromPrimary   = pack "From Aberdeen"

tableToTimetables :: Set Day -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables daySet direction body =
    Timetable { ferries   = handleOverMidnight $ mapMaybe toFerry $ findDirection (textForDirection direction) body
              , days      = daySet
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
