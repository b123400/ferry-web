module Timetable.Raw where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (evalStateT)
import Data.Cache (Cache, withCache)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (NominalDiffTime)

import Scraping.Class (Scrap, route)
import Timetable (Route, Island)

import qualified Scraping.Gov as Gov


allIslandsRaw :: IO [Route NominalDiffTime]
allIslandsRaw =
    flip evalStateT Gov.fetchCursor $ sequence [Gov.cheungChau, Gov.muiWo]
        -- TODO: More islands here

islandRaw :: Cache i => Scrap i => Proxy i -> IO (Route NominalDiffTime)
islandRaw = flip evalStateT Gov.fetchCursor . Gov.island
