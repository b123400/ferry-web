module Scraping.Gov where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Cache (Cache, withCache)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (NominalDiffTime)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)

import Scraping.Class (Scrap, route, shared)
import Scraping.Islands.CheungChau ()
import Scraping.Islands.MuiWo ()
import Scraping.Islands.PengChau ()
import Scraping.Islands.SokKwuWan ()
import Scraping.Islands.YungShueWan ()
import Timetable (Island(..), Route)


cheungChau :: MonadIO m => MonadCatch m => StateT (m Cursor) m (Route NominalDiffTime)
cheungChau = island (Proxy :: Proxy CheungChau)

muiWo :: MonadIO m => MonadCatch m => StateT (m Cursor) m (Route NominalDiffTime)
muiWo = island (Proxy :: Proxy MuiWo)

pengChau :: MonadIO m => MonadCatch m => StateT (m Cursor) m (Route NominalDiffTime)
pengChau = island (Proxy :: Proxy PengChau)

sokKwuWan :: MonadIO m => MonadCatch m => StateT (m Cursor) m (Route NominalDiffTime)
sokKwuWan = island (Proxy :: Proxy SokKwuWan)

yungShueWan :: MonadIO m => MonadCatch m => StateT (m Cursor) m (Route NominalDiffTime)
yungShueWan = island (Proxy :: Proxy YungShueWan)

fetchCursor :: IO Cursor
fetchCursor = do
    res <- simpleHttp "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/"
    pure $ fromDocument $ parseLBS res

island :: Cache i => Scrap i => MonadIO m => MonadCatch m => Proxy i -> StateT (m Cursor) m (Route NominalDiffTime)
island p = withCache p $ shared $ route p
