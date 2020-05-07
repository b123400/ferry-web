module Scraping.Gov.MaWanTsuenWan
( fetch
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cache (MonadCache, withCache)
import Text.XML.Cursor (Cursor, attributeIs, element, following,
                        ($.//), ($//), (>=>))
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack, unpack, isInfixOf)
import Text.Regex.TDFA ((=~))
import Timetable hiding (timetables)
import Scraping.Gov.TimeString (parseTimeStr)
import Scraping.Utility
import Data.Maybe
import Data.Time.Clock (NominalDiffTime)

import qualified Scraping.Gov as Gov (fetchCursor)

fetch :: (MonadIO m, MonadCache m ByteString, MonadCache m (Route NominalDiffTime)) => m (Route NominalDiffTime)
fetch = withCache "MaWanTsuenWan" $ do
    cursor <- Gov.fetchCursor
    pure $ Route MaWanTsuenWan $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findMaWanTsuenWan cursor
    direction <- [FromPrimary, ToPrimary]
    ct <- findTimetableCursors c
    catMaybes $ return $ findTimetable direction ct

findMaWanTsuenWan :: Cursor -> [Cursor]
findMaWanTsuenWan cursor = cursor $.// (element "a") >=> attributeIs "name" "o17"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    pure . nthMatch 3 (matchName "table") . following

findTimetable :: Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable direction timeTable =
    let ths = timeTable $// (element "th")
        trs = timeTable $// (element "tr")
        thTexts = flatContent <$> ths
        tdTexts = flatContent <$> (getTDs =<< filter hasTwoTd trs)
    in Just $ tableToTimetables direction (thTexts <> tdTexts)

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Tsuen Wan"
textForDirection FromPrimary   = pack "From Ma Wan"

tableToTimetables :: Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables direction body =
    Timetable { ferries   = handleOverMidnight $ mapMaybe toFerry $ findDirection (textForDirection direction) body
              , days      = everyday
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
