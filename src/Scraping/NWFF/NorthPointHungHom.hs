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
import Timetable (Day(..), Direction(..), Ferry(..), Modifier(..), Island(..), Timetable(..), weekdays, satSunAndHoliday)
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

data Days = MonToFri | SatSunAndHoliday deriving (Eq)

timetables :: Cursor -> [Timetable NominalDiffTime]
timetables cur = do
    days <- [SatSunAndHoliday, MonToFri]
    tables <- findTables cur
    tableToTimetables days tables

findTables :: Cursor -> [Cursor]
findTables cursor = cursor $.// element "table"

tableToTimetables :: Days -> Cursor -> [Timetable NominalDiffTime]
tableToTimetables days table =
    let ths = table $// (element "th")
        trs = table $// (element "tr")
        tuples = thsToTuple ths : mapMaybe trToTuple trs
        firsts = fst <$> tuples
        seconds = snd <$> tuples
        listsOfStrings = [firsts, seconds]
    in catMaybes $ do
            listOfStrings <- listsOfStrings
            direction <- [FromPrimary, ToPrimary]
            pure $ findTimetable days direction listOfStrings
    where
        trToTuple :: Cursor -> Maybe (Text, Text)
        trToTuple c = case getTDs c of
                          (first : second : [])-> Just (flatContent first, flatContent second)
                          _ -> Nothing
        thsToTuple (first : second : []) = (flatContent first, flatContent second)
        -- A default header just in case we cannot find the real direction
        thsToTuple _ = ("North Point -> Hung Hom", "Hung Hom -> North Point")

findTimetable :: Days -> Direction -> [Text] -> Maybe (Timetable NominalDiffTime)
findTimetable days directionWanted texts =
    if Just directionWanted == direction
        then Timetable <$> ferries <*> (pure daySet) <*> direction
        else Nothing
    where
        daySet = case days of MonToFri -> weekdays
                              SatSunAndHoliday -> satSunAndHoliday
        (directionStr : timeStrs) = texts

        parsedPairs = mapMaybe parseTimeStr timeStrs
        filteredDay = filter (\(m, _)-> notElem '#' m || days == MonToFri) parsedPairs
        toFerry (_, time) = Ferry time mempty

        ferries = case (toFerry <$> filteredDay) of
                      [] -> Nothing
                      a -> Just a

        -- Well HK island should be the island side but I want to be more consistent
        -- so "Leaving HK island = ToPrimary", sorry please send a PR with a better name
        direction = case directionStr of
                        "North Point -> Hung Hom" -> Just FromPrimary
                        "Hung Hom -> North Point" -> Just ToPrimary
                        _ -> Nothing
