module Scraping.Gov where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Cache (MonadCache, withCache)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)

fetchCursor :: (MonadCache m ByteString, MonadIO m) => m Cursor
fetchCursor = do
    res <- withCache "gov" $ simpleHttp "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/"
    pure $ fromDocument $ parseLBS res
