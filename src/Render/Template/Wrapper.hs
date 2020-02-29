{-# LANGUAGE QuasiQuotes #-}
module Render.Template.Wrapper where

import Lucid
import Text.RawString.QQ

wrapper_ :: Monad m => HtmlT m () -> HtmlT m ()
wrapper_ body = doctypehtml_ $ do
    head_ $ do
        meta_ [content_ "text/html;charset=utf-8", httpEquiv_ "Content-Type"]
        meta_ [content_ "utf-8", httpEquiv_ "encoding"]
        title_ "Ferry timetable"
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/style.css"]

    body_ body
