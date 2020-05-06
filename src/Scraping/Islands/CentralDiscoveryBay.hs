module Scraping.Islands.CentralDiscoveryBay
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
fetch = withCache "CentralDiscoveryBay" $ do
    cursor <- Gov.fetchCursor
    pure $ Route CentralDiscoveryBay $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cursor = do
    c <- findCentralDiscoveryBay cursor
    ct <- findTimetableCursors c
    cursorToTimetables ct

findCentralDiscoveryBay :: Cursor -> [Cursor]
findCentralDiscoveryBay cursor = cursor $.// (element "a") >=> attributeIs "name" "o11"

findTimetableCursors :: Cursor -> [Cursor]
findTimetableCursors =
    findTableElements . nthMatch 6 (matchName "table") . following

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
                Holiday          -> isInfixOf (pack "Sundays") text

textForDirection :: Direction -> Text
textForDirection ToPrimary = pack "From Discovery Bay"
textForDirection FromPrimary   = pack "From Central"

tableToTimetables :: Day -> Direction -> [Text] -> Timetable NominalDiffTime
tableToTimetables day direction body =
    Timetable { ferries   = handleOverMidnight $ catMaybes $ map (toFerry (isDay day)) $ findDirection (textForDirection direction) body
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
regexPattern = "([0-9]{1,2})[\\.:]([0-9]{1,2}) (a\\.m\\.|p\\.m\\.|noon)(\\*)?(@)?(#)?"

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
isDay Sunday   _ = True
isDay Holiday captures = (captures !! 6) /= "#"
isDay Weekday  _ = True
isDay Saturday _ = True

capturesToFerry :: [String] -> (Ferry NominalDiffTime)
capturesToFerry captures =
    Ferry { time      = fromInteger $ ((if isAm then hours else hours + 12) * 60 + minutes) * 60
          , modifiers = mempty
          }
    where hours   = read (captures !! 1) `mod` 12
          minutes = read (captures !! 2)
          isAm    = (captures !! 3) == "a.m."
          -- isViaDisneyland  = (captures !! 4) == "*"
          -- The sailing will operate via Disneyland Resort Pier (Due to repair work in progress, the stopping point at Disneyland Resort Pier is temporary suspended.)
