module Scraping.Islands.MaWanTsuenWan
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
    ct <- findTimetableCursors c
    cursorToTimetables ct

findMaWanTsuenWan :: Cursor -> [Cursor]
findMaWanTsuenWan cursor = cursor $.// (element "a") >=> attributeIs "name" "o17"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    pure . nthMatch 3 (matchName "table") . following

cursorToTimetables :: Cursor -> [Timetable NominalDiffTime]
cursorToTimetables timeTable = catMaybes $ do
    day <- [Weekday, Saturday, Sunday, Holiday]
    direction <- [FromPrimary, ToPrimary]
    return $ findTimetable day direction timeTable

findTimetable :: Day -> Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable day direction timeTable =
    let ths = timeTable $// (element "th")
        trs = timeTable $// (element "tr")
        thTexts = flatContent <$> ths
        tdTexts = flatContent <$> (getTDs =<< filter hasTwoTd trs)
    in Just $ tableToTimetables day direction (thTexts <> tdTexts)

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Tsuen Wan"
textForDirection FromPrimary   = pack "From Ma Wan"

tableToTimetables :: Day -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables day direction body =
    Timetable { ferries   = handleOverMidnight $ map toFerry $ findDirection (textForDirection direction) body
              , day       = day
              , direction = direction
              }

findDirection :: Text -> [Text] -> [Text]
findDirection keyword list
    | isInfixOf keyword (list !! 0) = filter notEmpty $ tail $ pickOdd list
    | isInfixOf keyword (list !! 1) = filter notEmpty $ tail $ pickEven list
    | otherwise                     = error ("not found")

{-
Match
1.20 a.m.
12.30 p.m.
12.00 noon
13.30 p.m.*
14.30 p.m.*@
15.30 p.m.#
-}
regexPattern :: String
regexPattern = "([0-9]{1,2})[\\.:]([0-9]{1,2}) (a\\.m\\.|p\\.m\\.|noon)"

splitCapture :: String -> [String]
splitCapture timeString
    | (length matches) == 1 = head matches
    | otherwise             = error ("regex error " ++ timeString)
    where matches = (cleanHTMLEntity timeString =~ regexPattern)

toFerry :: Text -> Ferry NominalDiffTime
toFerry text =
    let captures = splitCapture $ unpack text
    in capturesToFerry captures

capturesToFerry :: [String] -> (Ferry NominalDiffTime)
capturesToFerry captures =
    Ferry { time      = fromInteger $ ((if isAm then hours else hours + 12) * 60 + minutes) * 60
          , modifiers = mempty
          }
    where hours   = read (captures !! 1) `mod` 12
          minutes = read (captures !! 2)
          isAm    = (captures !! 3) == "a.m."
