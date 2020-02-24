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
import Timetable (Route, Island)

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
        [ Gov.cheungChau
        , Gov.muiWo
        , Gov.pengChau
        , Gov.sokKwuWan
        , Gov.yungShueWan
        ]
        -- TODO: More islands here

islandRaw :: LC.Cache i => Scrap i => Cache String (Route NominalDiffTime) -> Proxy i -> IO (Route NominalDiffTime)
islandRaw cache = flip evalStateT (cache, Gov.fetchCursor) . Gov.island
