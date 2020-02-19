module Render.Template.Route where

import Lucid
import Control.Monad (forM_)
import Data.Time.LocalTime (LocalTime)
import Timetable (Route(..))
import Render.Template.Timetable (DisplayTime, timetable_)

route_ :: DisplayTime t => Monad m => Route t -> HtmlT m ()
route_ (Route island timetables) =
    div_ [class_ "island"] $ do
        h1_ (toHtml $ show island)
        div_ [class_ "direction"] $ do
            forM_ timetables timetable_
