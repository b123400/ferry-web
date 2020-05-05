module Render.Template.Route where

import Lucid
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)
import Timetable (Route(..))
import Render.Lang (Lang, lShow)
import Render.Template.Timetable (timetable_)
import Render.Template.Ferry (DisplayTime)

route_ :: DisplayTime t => Monad m => Lang -> Route t -> HtmlT m ()
route_ l (Route island timetables) =
    div_ [class_ "island --main"] $ do
        h1_ (lShow l island)
        forM_ timetables $ \t -> do
            div_ [class_ "direction"] $ do
                timetable_ l t
        div_ [class_ "clearfix"] $ pure ()
