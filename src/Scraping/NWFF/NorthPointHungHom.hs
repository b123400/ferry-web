module Scraping.NWFF.NorthPointHungHom (fetch) where

import Control.Monad.Catch (MonadCatch, handleAll)
import Control.Monad.Cache (MonadCache, withCache)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Map.Strict as Map (Map)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Network.HTTP.Conduit (simpleHttp)
import Text.XML.Cursor (Cursor, attributeIs, element, following, ($/), ($//), ($.//))
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)

import Scraping.NWFF.RouteScript (northPointHungHom)
import Scraping.NWFF.TimeString (parseTimeStr)
import Scraping.Utility
import Timetable (Day(..), Direction(..), Ferry(..), FerryType(..), Island(..), Timetable(..))
import qualified Timetable as T (Route(..))


fetch :: (MonadIO m, MonadCatch m, MonadCache m (Map String String), MonadCache m (T.Route NominalDiffTime)) => m (T.Route NominalDiffTime)
fetch =  withCache "NorthPointHungHom" $ do
    t <- fetchTimetables
    pure $ T.Route NorthPointHungHom t


fetchTimetables :: (MonadCache m (Map String String), MonadIO m, MonadCatch m) => m [Timetable NominalDiffTime]
fetchTimetables = do
    url <- northPointHungHom
    res <- simpleHttp url
    let cursor = fromDocument $ parseLBS res
    pure $ timetables cursor

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cur = tableToTimetables =<< findTables cur

findTables :: Cursor -> [Cursor]
findTables cursor = cursor $.// element "table"

tableToTimetables :: Cursor -> [Timetable NominalDiffTime]
tableToTimetables table =
    let ths = table $// (element "th")
        trs = table $// (element "tr")
        tuples = thsToTuple ths : mapMaybe trToTuple trs
        firsts = fst <$> tuples
        seconds = snd <$> tuples
        listsOfStrings = [firsts, seconds]
    in catMaybes $ do
            listOfStrings <- listsOfStrings
            day <- [Weekday, Saturday, Sunday, Holiday]
            direction <- [ToIsland, FromIsland]
            pure $ findTimetable day direction listOfStrings
    where
        trToTuple :: Cursor -> Maybe (Text, Text)
        trToTuple c = case getTDs c of
                          (first : second : [])-> Just (flatContent first, flatContent second)
                          _ -> Nothing
        thsToTuple (first : second : []) = (flatContent first, flatContent second)
        -- A default header just in case we cannot find the real direction
        thsToTuple _ = ("North Point -> Hung Hom", "Hung Hom -> North Point")

findTimetable :: Day -> Direction -> [Text] -> Maybe (Timetable NominalDiffTime)
findTimetable day directionWanted texts =
    if Just directionWanted == direction
        then Timetable <$> ferries <*> (pure day) <*> direction
        else Nothing
    where
        (directionStr : timeStrs) = texts

        parsedPairs = mapMaybe parseTimeStr timeStrs
        filteredDay = filter (\(m, _)-> notElem '#' m || day == Weekday) parsedPairs
        toFerry (_, time) = Ferry time SlowFerry

        ferries = case (toFerry <$> filteredDay) of
                      [] -> Nothing
                      a -> Just a

        -- Well HK island should be the island side but I want to be more consistent
        -- so "Leaving HK island = FromIsland", sorry please send a PR with a better name
        direction = case directionStr of
                        "North Point -> Hung Hom" -> Just ToIsland
                        "Hung Hom -> North Point" -> Just FromIsland
                        _ -> Nothing
