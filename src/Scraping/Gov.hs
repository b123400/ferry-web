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
import Scraping.Islands.CheungChau ()
import Scraping.Islands.MuiWo ()
import Scraping.Islands.PengChau ()
import Scraping.Islands.SokKwuWan ()
import Scraping.Islands.YungShueWan ()
import Timetable (Island(..), Route)


cheungChau :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
cheungChau = island (Proxy :: Proxy CheungChau)

muiWo :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
muiWo = island (Proxy :: Proxy MuiWo)

pengChau :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
pengChau = island (Proxy :: Proxy PengChau)

sokKwuWan :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
sokKwuWan = island (Proxy :: Proxy SokKwuWan)

yungShueWan :: MonadIO m => MonadCatch m => HasCursor a => HasCache a (Route NominalDiffTime) => StateT a m (Route NominalDiffTime)
yungShueWan = island (Proxy :: Proxy YungShueWan)

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
