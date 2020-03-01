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
    div_ [class_ "island --main"] $ do
        h1_ (toHtml $ islandName island)
        forM_ timetables $ \t -> do
            div_ [class_ "direction"] $ do
                timetable_ t
        div_ [class_ "clearfix"] $ pure ()
