module Render.Page.Index where

import Servant
import Lucid
import Control.Monad (forM_)
import Data.Aeson (ToJSON(..))
import Data.Time.LocalTime (LocalTime)
import Render.Lang (Localised(..))
import Render.Template.Route (route_)
import Render.Template.Wrapper (wrapper_)
import Timetable (Route(..))
import Timetable.Local (addDiff)
import Web.HttpApiData (toUrlPiece)

data Index = Index
    { now :: LocalTime
    , routes :: [Route LocalTime]
    }


instance ToHtml (Localised Index) where
    toHtmlRaw = toHtml
    toHtml (Localised l (Index now routes)) = wrapper_ l $ do
        forM_ withDiffs $ \route@(Route island _) -> do
            a_ [href_ $ toUrlPiece island] $ do
                route_ l route
        div_ [class_ "clearfix"] $ pure ()
        where withDiffs = fmap (addDiff now) <$> routes

instance ToJSON (Localised Index) where
    toJSON (Localised l (Index _ routes)) = toJSON routes
