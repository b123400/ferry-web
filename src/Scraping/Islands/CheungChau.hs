module Scraping.Islands.CheungChau
( timetables, route
) where

import Text.XML.Cursor (Cursor, attributeIs, child, following,
                        ($.//), ($//), (>=>))
import Data.Text (Text, pack, unpack, isInfixOf)
import Text.Regex.TDFA ((=~))
import Timetable hiding (timetables)
import Scraping.Utility
import Data.Maybe
import Data.Time.Clock (NominalDiffTime)
import Debug.Trace

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCheungChau cursor
    ct <- findTimetableCursors c
    cursorToTimetables ct


route :: Cursor -> Route NominalDiffTime
route cursor = Route CheungChau $ timetables cursor

findCheungChau :: Cursor -> [Cursor]
findCheungChau cursor = cursor $.// (makeElement "a") >=> attributeIs (makeName "name") (Data.Text.pack "o01")

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    findTableElements . nthMatch 5 (matchName "table") . following

findTableElements :: Cursor -> [Cursor]
findTableElements c = c $// (makeElement "table")

cursorToTimetables :: Cursor -> [Timetable NominalDiffTime]
cursorToTimetables timeTable =
    catMaybes $
    [Weekday, Saturday, Holiday] >>= \day ->
    [ToIsland, FromIsland]       >>= \direction ->
    return $ findTimetable day direction timeTable

findTimetable :: Day -> Direction -> Cursor -> Maybe (Timetable NominalDiffTime)
findTimetable day direction timeTable
    | not $ hasDay timeTable day = Nothing
    | otherwise =
        let trs = timeTable $// (makeElement "tr")
        in Just $ tableToTimetables day direction (flatContent <$> (child =<< getTDs =<< filter hasTwoTd trs))

hasDay :: Cursor -> Day -> Bool
hasDay cursor day =
    case cursor $// (makeElement "td") of
        (x:_) -> textHasDay day $ flatContent x
        _ -> False
    where
        textHasDay :: Day -> Text -> Bool
        textHasDay day text =
            case day of
                Weekday  -> isInfixOf (pack "Mondays") text
                Saturday -> isInfixOf (pack "Saturdays") text
                Holiday  -> isInfixOf (pack "Sundays") text

textForDirection :: Direction -> Text
textForDirection FromIsland = pack "From Cheung Chau"
textForDirection ToIsland   = pack "From Central"

tableToTimetables :: Day -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables day direction body =
    Timetable { ferries   = catMaybes $ map (toFerry (isDay day)) $ findDirection (textForDirection direction) body
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
regexPattern = "([0-9]{1,2})\\.([0-9]{1,2}) ((a|p)\\.m\\.|noon)(\\*)?(@)?(#)?"

splitCapture :: String -> [String]
splitCapture timeString
    | (length matches) == 1 = head matches
    | otherwise             = error ("regex error " ++ timeString)
    where matches = (cleanHTMLEntity timeString =~ regexPattern)

toFerry :: ([String] -> Bool) -> Text -> Maybe (Ferry NominalDiffTime)
toFerry cond text =
    let captures = splitCapture $ unpack text
    in if cond captures
       then Just (capturesToFerry captures)
       else Nothing


isDay :: Day -> [String] -> Bool
isDay Holiday  _        = True
isDay Weekday  captures = (captures !! 6) /= "@"
isDay Saturday captures = (captures !! 7) /= "#"

capturesToFerry :: [String] -> (Ferry NominalDiffTime)
capturesToFerry captures =
    Ferry { time      = fromInteger $ ((if isAm then hours else hours + 12) * 60 + minutes) * 60
          , ferryType = if   isSlow
                        then SlowFerry
                        else FastFerry
          }
    where hours   = read (captures !! 1) `mod` 12
          minutes = read (captures !! 2)
          isAm    = (captures !! 3) == "a.m."
          isSlow  = (captures !! 5) == "*"
