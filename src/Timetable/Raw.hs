module Timetable.Raw where

import Control.Monad.Cache (MonadCache)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict as Map (Map)
import Data.Time.Clock (NominalDiffTime)

import Timetable (Route, Island(..))
import qualified Scraping.Islands.CentralCheungChau
import qualified Scraping.Islands.CentralMuiWo
import qualified Scraping.Islands.CentralPengChau
import qualified Scraping.Islands.CentralSokKwuWan
import qualified Scraping.Islands.CentralYungShueWan
import qualified Scraping.NWFF.NorthPointHungHom


allIslandsRaw
    :: ( MonadIO m
       , MonadCatch m
       , MonadCache m ByteString
       , MonadCache m (Route NominalDiffTime)
       , MonadCache m (Map String String)
        )
    => m [Route NominalDiffTime]
allIslandsRaw = sequence
        [ islandRaw CentralCheungChau
        , islandRaw CentralMuiWo
        , islandRaw CentralPengChau
        , islandRaw CentralSokKwuWan
        , islandRaw CentralYungShueWan
        , islandRaw NorthPointHungHom
        ]
        -- TODO: More islands here

islandRaw
    :: ( MonadIO m
       , MonadCatch m
       , MonadCache m ByteString
       , MonadCache m (Route NominalDiffTime)
       , MonadCache m (Map String String)
       )
    => Island-> m (Route NominalDiffTime)
islandRaw CentralCheungChau = Scraping.Islands.CentralCheungChau.fetch
islandRaw CentralMuiWo = Scraping.Islands.CentralMuiWo.fetch
islandRaw CentralPengChau = Scraping.Islands.CentralPengChau.fetch
islandRaw CentralSokKwuWan = Scraping.Islands.CentralSokKwuWan.fetch
islandRaw CentralYungShueWan = Scraping.Islands.CentralYungShueWan.fetch
islandRaw NorthPointHungHom = Scraping.NWFF.NorthPointHungHom.fetch
