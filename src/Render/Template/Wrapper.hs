{-# LANGUAGE QuasiQuotes #-}
module Render.Template.Wrapper where

import Lucid
import Text.RawString.QQ
import Render.Lang (Lang(..))

wrapper_ :: Monad m => Lang -> HtmlT m () -> HtmlT m ()
wrapper_ l body = do
    doctype_
    html_ [lang_ currentLang] $ do
        head_ $ do
            meta_ [content_ "text/html;charset=utf-8", httpEquiv_ "Content-Type"]
            meta_ [content_ "utf-8", httpEquiv_ "encoding"]
            title_ "Ferry timetable"
            link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/style.css"]

        body_ $ do
            language
            body

        where
            currentLang = case l of
                En -> "en"
                Hk -> "zh-Hant-HK"
            language = case l of
                En -> a_ [class_ "lang", href_ "/lang/hk"] "🇭🇰"
                Hk -> a_ [class_ "lang", href_ "/lang/en"] "🇬🇧"
