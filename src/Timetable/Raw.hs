module Timetable.Raw where

import Control.Lens (_1, _2)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (evalStateT)
import Data.Cache (Cache)
import Data.MemoryCache (HasCache(..))
import Data.Proxy (Proxy(..))
import Data.Time.Clock (NominalDiffTime)
import Text.XML.Cursor (Cursor)

import Scraping.Class (Scrap, HasCursor(..), route)
import Timetable (Route, Island(..))

import qualified Data.LocalCache as LC (Cache)
import qualified Scraping.Gov as Gov

type State = (Cache String (Route NominalDiffTime), IO Cursor)

instance HasCache State (Route NominalDiffTime) where
    cacheL = _1

instance HasCursor State where
    cursorL = _2

allIslandsRaw :: Cache String (Route NominalDiffTime) -> IO [Route NominalDiffTime]
allIslandsRaw cache =
    flip evalStateT (cache, Gov.fetchCursor) $ sequence
        [ Gov.centralCheungChau
        , Gov.centralMuiWo
        , Gov.centralPengChau
        , Gov.centralSokKwuWan
        , Gov.centralYungShueWan
        ]
        -- TODO: More islands here

islandRaw :: LC.Cache i => Scrap i => Cache String (Route NominalDiffTime) -> Proxy i -> IO (Route NominalDiffTime)
islandRaw cache = flip evalStateT (cache, Gov.fetchCursor) . Gov.island

islandRaw' :: Cache String (Route NominalDiffTime) -> Island -> IO (Route NominalDiffTime)
islandRaw' cache island = case island of
    CentralCheungChau -> islandRaw cache (Proxy :: Proxy CentralCheungChau)
    CentralMuiWo -> islandRaw cache (Proxy :: Proxy CentralMuiWo)
    CentralPengChau -> islandRaw cache (Proxy :: Proxy CentralPengChau)
    CentralSokKwuWan -> islandRaw cache (Proxy :: Proxy CentralSokKwuWan)
    CentralYungShueWan -> islandRaw cache (Proxy :: Proxy CentralYungShueWan)
