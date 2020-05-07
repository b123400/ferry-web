module Render.Page.RawTimetable where

import Servant
import Lucid
import Control.Monad (forM_)
import Data.Aeson (ToJSON(..))
import Data.Time.Clock (NominalDiffTime)
import Render.Lang as L (Localised(..), translate, lShow)
import qualified Render.Lang as L
import Render.Template.Wrapper (wrapper_)
import Render.Template.RawTimetable (rawTimetable_)
import Timetable (Route(..), Timetable(..), Day(..))

data RawTimetable = RawTimetable
    { route :: Route NominalDiffTime
    }

instance ToHtml (Localised RawTimetable) where
    toHtmlRaw = toHtml
    toHtml (Localised l (RawTimetable (Route island timetables))) = wrapper_ l $ do
        div_ [class_ "island --raw"] $ do
            h1_ (lShow l island)
            -- TODO: group by same days but different directions
            forM_ timetables $ \timetable@(Timetable _ days _) -> do
                div_ [class_ "day"] $ do
                    -- TODO: render day set to proper string
                    h2_ (toHtml $ show days) --(t L.Sunday)
                    div_ [class_ "direction"] $ do
                        rawTimetable_ l timetable
                    div_ [class_ "clearfix"] $ pure ()
            div_ [class_ "clearfix"] $ pure ()

        where
              t = translate l


instance ToJSON (Localised RawTimetable) where
    toJSON (Localised l (RawTimetable route)) = toJSON route
