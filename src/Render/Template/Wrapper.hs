{-# LANGUAGE QuasiQuotes #-}
module Render.Template.Wrapper where

import Lucid
import Text.RawString.QQ

wrapper_ :: Monad m => HtmlT m () -> HtmlT m ()
wrapper_ body = doctypehtml_ $ do
    head_ $ do
        meta_ [content_ "text/html;charset=utf-8", httpEquiv_ "Content-Type"]
        meta_ [content_ "utf-8", httpEquiv_ "encoding"]
        title_ "Title"
        style_ [r|
            body {
                padding-left: 15%;
                margin: 0 auto;
                font-family: -apple-system, Helvetica, sans-serif;
            }
            .island {
                margin-bottom: 60px;
            }
            h1 {
                font-weight: 200;
            }
            h2 {
                font-weight: 600;
                font-size: 1em;
            }
            ol {
                list-style: none;
                border-left: solid 1px black;
                padding-left: 10px;
                margin-left: 10px;
            }
            .timeslot {
                padding: 5px 10px;
                position: relative;
            }
            time {
                font-size: 2em;
            }
            .fast-ferry {
                background: red;
                height: 70%;
                width: 5px;
                display: block;
                position: absolute;
                top: 15%;
                left: 0;
            }
            .slow-ferry {
                background: green;
                height: 70%;
                width: 5px;
                display: block;
                position: absolute;
                top: 15%;
                left: 0;
            }
            .reminder {
                color: gray;
            }
            .clearfix {
                clear: both;
            }
            @media (min-width: 700px) {
                .island {
                    float: left;
                    width: 50%;
                    max-width: 350px;
                    min-width: 250px;
                }
            }
        |]

    body_ body