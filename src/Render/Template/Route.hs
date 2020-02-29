module Render.Template.Route where

import Lucid
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)
import Timetable (Route(..), islandName)
import Render.Template.Timetable (timetable_)
import Render.Template.Ferry (DisplayTime)

route_ :: DisplayTime t => Monad m => Route t -> HtmlT m ()
route_ (Route island timetables) =
    div_ [class_ "island"] $ do
        h1_ (toHtml $ islandName island)
        forM_ timetables $ \t -> do
            div_ [class_ "direction"] $ do
                timetable_ t
        div_ [class_ "clearfix"] $ pure ()

detailRoute_ :: DisplayTime t => Monad m => Route t -> HtmlT m ()
detailRoute_ (Route island timetables) = do
    div_ [class_ "island --detail"] $ do
        h1_ (toHtml $ islandName island)
        forM_ timetables $ \t -> do
            div_ [class_ "direction"] $ do
                timetable_ t
        div_ [class_ "clearfix"] $ pure ()
    div_ [class_ "sidebar"] $ do
        a_ [href_ ""] "Now"
        br_ []
        br_ []
        form_ [] $ do
            "Select date:"
            br_ []
            input_ [type_ "date"]
            br_ []
            input_ [type_ "submit", value_ "Submit"]
        br_ []
        br_ []
        a_ [href_ ""] "Raw timetable"
