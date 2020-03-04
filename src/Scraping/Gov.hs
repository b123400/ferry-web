module Scraping.Gov where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.LocalCache (Cache, withLocalCache)
import Data.MemoryCache (HasCache, withMemoryCache)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (NominalDiffTime)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)

import Scraping.Class (Scrap, HasCursor, route, shared)
import Scraping.Islands.CentralCheungChau ()
import Scraping.Islands.CentralMuiWo ()
import Scraping.Islands.CentralPengChau ()
import Scraping.Islands.CentralSokKwuWan ()
import Scraping.Islands.CentralYungShueWan ()
import Timetable (Island(..), Route)


centralCheungChau :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
centralCheungChau = island (Proxy :: Proxy CentralCheungChau)

centralMuiWo :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
centralMuiWo = island (Proxy :: Proxy CentralMuiWo)

centralPengChau :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
centralPengChau = island (Proxy :: Proxy CentralPengChau)

centralSokKwuWan :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
centralSokKwuWan = island (Proxy :: Proxy CentralSokKwuWan)

centralYungShueWan :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
centralYungShueWan = island (Proxy :: Proxy CentralYungShueWan)

fetchCursor :: IO Cursor
fetchCursor = do
    res <- simpleHttp "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/"
    pure $ fromDocument $ parseLBS res

island
    :: Cache i
    => Scrap i
    => MonadIO m
    => MonadCatch m
    => HasCursor a
    => HasCache a (Route NominalDiffTime)
    => Proxy i
    -> StateT a m (Route NominalDiffTime)
island p = withMemoryCache p $ withLocalCache p $ shared $ route p
