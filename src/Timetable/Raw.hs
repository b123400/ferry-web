module Timetable.Raw where

import Control.Monad.Cache (MonadCache)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict as Map (Map)
import Data.Time.Clock (NominalDiffTime)

import Timetable (Route, Island(..))
import qualified Scraping.GovData.CentralCheungChau
import qualified Scraping.Gov.CentralMuiWo
import qualified Scraping.Gov.CentralPengChau
import qualified Scraping.Gov.CentralSokKwuWan
import qualified Scraping.Gov.CentralYungShueWan
import qualified Scraping.Gov.PengChauHeiLingChau
import qualified Scraping.Gov.AberdeenSokKwuWan
import qualified Scraping.GovData.CentralDiscoveryBay
import qualified Scraping.Gov.MaWanTsuenWan
import qualified Scraping.NWFF.NorthPointHungHom
import qualified Scraping.NWFF.NorthPointKowloonCity
import qualified Scraping.CoralSea.SaiWanHoKwunTong
import qualified Scraping.CoralSea.SaiWanHoSamKaTsuen
import qualified Scraping.CoralSea.SamKaTsuenTungLungIsland


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
        , islandRaw NorthPointKowloonCity
        , islandRaw PengChauHeiLingChau
        , islandRaw AberdeenSokKwuWan
        , islandRaw CentralDiscoveryBay
        , islandRaw MaWanTsuenWan
        , islandRaw SaiWanHoKwunTong
        , islandRaw SaiWanHoSamKaTsuen
        , islandRaw SamKaTsuenTungLungIsland
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
islandRaw CentralCheungChau = Scraping.GovData.CentralCheungChau.fetch
islandRaw CentralMuiWo = Scraping.Gov.CentralMuiWo.fetch
islandRaw CentralPengChau = Scraping.Gov.CentralPengChau.fetch
islandRaw CentralSokKwuWan = Scraping.Gov.CentralSokKwuWan.fetch
islandRaw CentralYungShueWan = Scraping.Gov.CentralYungShueWan.fetch
islandRaw NorthPointHungHom = Scraping.NWFF.NorthPointHungHom.fetch
islandRaw NorthPointKowloonCity = Scraping.NWFF.NorthPointKowloonCity.fetch
islandRaw PengChauHeiLingChau = Scraping.Gov.PengChauHeiLingChau.fetch
islandRaw AberdeenSokKwuWan = Scraping.Gov.AberdeenSokKwuWan.fetch
islandRaw CentralDiscoveryBay = Scraping.GovData.CentralDiscoveryBay.fetch
islandRaw MaWanTsuenWan = Scraping.Gov.MaWanTsuenWan.fetch
islandRaw SaiWanHoKwunTong = Scraping.CoralSea.SaiWanHoKwunTong.fetch
islandRaw SaiWanHoSamKaTsuen = Scraping.CoralSea.SaiWanHoSamKaTsuen.fetch
islandRaw SamKaTsuenTungLungIsland = Scraping.CoralSea.SamKaTsuenTungLungIsland.fetch
