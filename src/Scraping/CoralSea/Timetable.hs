module Scraping.CoralSea.Timetable where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Cache (MonadCache, withCache)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (httpLbs, tlsManagerSettings, newManager, parseRequest, requestHeaders, responseBody)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)

fetchCursor :: (MonadCache m ByteString, MonadIO m, MonadThrow m) => m Cursor
fetchCursor = fromDocument <$> parseLBS <$> fetch

fetch :: (MonadCache m ByteString, MonadIO m, MonadThrow m) => m ByteString
fetch = withCache "CoralSea" $ do
    manager <- liftIO $ newManager tlsManagerSettings
    initReq <- parseRequest "https://www.coralseaferryservice.com.hk/timetable"
    let req = initReq { requestHeaders = [("Content-Type", "text/html"), ("Accept", "text/html")] }
    res <- httpLbs req manager
    pure $ responseBody res
