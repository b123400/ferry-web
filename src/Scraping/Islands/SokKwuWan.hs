module Scraping.Islands.SokKwuWan
( timetables
) where

import Text.XML.Cursor (Cursor, attributeIs, following,
                        ($.//), ($//), ($/), (>=>))
import Data.Cache (Cache(..))
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack, isInfixOf)
import Data.Time.Clock (NominalDiffTime)
import Scraping.Class (Scrap(..))
import Text.Regex.TDFA ((=~))
import Timetable hiding (timetables)
import Scraping.Utility

instance Cache SokKwuWan where
    cacheFilename _ = "SokKwuWan"

instance Scrap SokKwuWan where
    route _ cursor = Route SokKwuWan $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findSokKwuWan cursor
    ct <- findTimetableCursors c
    cursorToTimetables ct


findSokKwuWan :: Cursor -> [Cursor]
findSokKwuWan cursor = cursor $.// (makeElement "a") >=> attributeIs (makeName "name") (Data.Text.pack "o05")

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors = findTableElements . nthMatch 3 (matchName "table") . following

findTableElements :: Cursor -> [Cursor]
findTableElements c = c $// (makeElement "table")

cursorToTimetables :: Cursor -> [Timetable NominalDiffTime]
cursorToTimetables timeTable = catMaybes $ do
    day <- [Weekday, Saturday, SundayAndHoliday]
    direction <- [ToIsland, FromIsland]
    return $ findTimetable day direction timeTable

findTimetable :: Day -> Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable day direction timeTable
    | not $ hasDay timeTable day = Nothing
    | otherwise                  =
        let trs = timeTable $// (makeElement "tr")
        in Just $ tableToTimetables day direction $ flatContent <$> (getTDs =<< filter hasTwoTd trs)

hasDay :: Cursor -> Day -> Bool
hasDay cursor day = textHasDay day $ flatContent $ head $ cursor $// (makeElement "td")

textHasDay :: Day -> Text -> Bool
textHasDay day text = isInfixOf (pack(
    case day of
        Weekday  -> "Mondays"
        Saturday -> "Saturdays"
        SundayAndHoliday  -> "Sundays"
    )) text

textForDirection :: Direction -> Text
textForDirection FromIsland = pack "From Sok Kwu Wan"
textForDirection ToIsland   = pack "From Central"

tableToTimetables :: Day -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables day direction body =
    Timetable { ferries   = map toFerry $ findDirection (textForDirection direction) body
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
-}
regexPattern :: String
regexPattern = "([0-9]{1,2})\\.([0-9]{1,2}) ((a|p)\\.m\\.|noon)"

splitCapture :: String -> [String]
splitCapture timeString
    | (length matches) == 1 = head matches
    | otherwise             = error ("regex error " ++ timeString)
    where matches = (cleanHTMLEntity timeString =~ regexPattern)

toFerry :: Text -> Ferry NominalDiffTime
toFerry = capturesToFerry . splitCapture . unpack

capturesToFerry :: [String] -> Ferry NominalDiffTime
capturesToFerry captures =
    Ferry { time      = fromInteger $ ((if isAm then hours else hours + 12) * 60 + minutes) * 60
          , ferryType = if   isSlow
                        then SlowFerry
                        else FastFerry
          }
    where hours   = read (captures !! 1) `mod` 12
          minutes = read (captures !! 2)
          isAm    = (captures !! 3) == "a.m."
          isSlow  = True
