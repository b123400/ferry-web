module Render.Page.Detail where

import Servant
import Lucid
import Control.Monad (forM_)
import Data.Aeson (ToJSON(..))
import Data.Text (pack)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)
import Render.Template.Wrapper (wrapper_)
import Render.Template.Timetable (timetable_)
import Timetable (Route(..), islandName)
import Timetable.Local (addDiff)
import Web.HttpApiData (toUrlPiece)

data Detail = Detail
    { now :: LocalTime
    , route :: Route LocalTime
    , date :: Day
    , count :: Int
    }


instance ToHtml Detail where
    toHtmlRaw = toHtml
    toHtml (Detail now route date count) = wrapper_ $ do
        div_ [class_ "island --detail"] $ do
            h1_ (toHtml $ islandName island)
            forM_ timetables $ \t -> do
                div_ [class_ "direction"] $ do
                    timetable_ t
            div_ [class_ "clearfix"] $ pure ()
        div_ [class_ "sidebar"] $ do
            a_ [href_ $ "/" <> toUrlPiece island] "Now"
            br_ []
            br_ []
            form_ [method_ "get"] $ do
                label_ [for_ "date"] "Select date:"
                br_ []
                input_ [type_ "date", name_ "date", value_ (toQueryParam date)]
                br_ []
                label_ [for_ "count"] "Items per page"
                br_ []
                select_ [name_ "count"] $ do
                    option 10
                    option 20
                    option 50
                br_ []
                input_ [type_ "submit", value_ "Submit"]
            br_ []
            br_ []
            a_ [href_ $ "/" <> toUrlPiece island <> "/raw"] "Raw timetable"

        where withDiffs = addDiff now <$> route
              (Route island timetables) = withDiffs
              option c | c == count = option_ [value_ (pack $ show c), selected_ "true"] $ toHtml $ show c
                       | otherwise = option_ [value_ (pack $ show c)] $ toHtml $ show c

instance ToJSON Detail where
    toJSON (Detail _ route _ _) = toJSON route
