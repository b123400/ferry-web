module Render.Html where

import Servant
import Lucid
import Control.Monad (forM_)
import Data.Time.LocalTime (LocalTime)
import Network.HTTP.Media ((//), (/:))
import Timetable (Route)

import Render.Template.Wrapper (wrapper_)
import Render.Template.Route (route_)
import Render.Template.Ferry (DisplayTime)

data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS
