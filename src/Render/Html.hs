module Render.Html where

import Servant
import Lucid
import Control.Monad (forM_)
import Data.Time.LocalTime (LocalTime)
import Network.HTTP.Media ((//), (/:))
import Timetable (Route)

import Render.Template.Wrapper (wrapper_)
import Render.Template.Route (route_)
import Render.Template.Timetable (DisplayTime)

data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS


-- HTML serialization of a single person
-- instance ToHtml (Route LocalTime) where
--     toHtml route =
--         tr_ $ do
--             td_ "wow1"
--             td_ "wow2"
--       -- td_ (toHtml $ firstName person)
--       -- td_ (toHtml $ lastName person)

--   -- do not worry too much about this
--     toHtmlRaw = toHtml

instance DisplayTime t => ToHtml [Route t] where
    toHtml routes = wrapper_ $ do
        forM_ routes route_

    -- table_ $ do
    -- tr_ $ do
    --   th_ "hello"
    --   th_ "world"

    -- foldMap toHtml routes

    toHtmlRaw = toHtml
