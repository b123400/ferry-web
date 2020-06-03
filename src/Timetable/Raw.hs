module Timetable.Raw where

import Control.Monad.Cache (MonadCache)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict as Map (Map)
import Data.Time.Clock (NominalDiffTime)

import Timetable (Route, Island(..))
import qualified Scraping.GovData.CentralCheungChau
import qualified Scraping.GovData.CentralMuiWo
import qualified Scraping.GovData.CentralPengChau
import qualified Scraping.Gov.PengChauHeiLingChau
import qualified Scraping.GovData.CentralSokKwuWan
import qualified Scraping.GovData.CentralYungShueWan
import qualified Scraping.Gov.AberdeenSokKwuWan
import qualified Scraping.GovData.CentralDiscoveryBay
import qualified Scraping.GovData.MaWanTsuenWan
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
islandRaw CentralMuiWo = Scraping.GovData.CentralMuiWo.fetch
islandRaw CentralPengChau = Scraping.GovData.CentralPengChau.fetch
islandRaw CentralSokKwuWan = Scraping.GovData.CentralSokKwuWan.fetch
islandRaw CentralYungShueWan = Scraping.GovData.CentralYungShueWan.fetch
islandRaw NorthPointHungHom = Scraping.NWFF.NorthPointHungHom.fetch
islandRaw NorthPointKowloonCity = Scraping.NWFF.NorthPointKowloonCity.fetch
islandRaw PengChauHeiLingChau = Scraping.Gov.PengChauHeiLingChau.fetch
islandRaw AberdeenSokKwuWan = Scraping.Gov.AberdeenSokKwuWan.fetch
islandRaw CentralDiscoveryBay = Scraping.GovData.CentralDiscoveryBay.fetch
islandRaw MaWanTsuenWan = Scraping.GovData.MaWanTsuenWan.fetch
islandRaw SaiWanHoKwunTong = Scraping.CoralSea.SaiWanHoKwunTong.fetch
islandRaw SaiWanHoSamKaTsuen = Scraping.CoralSea.SaiWanHoSamKaTsuen.fetch
islandRaw SamKaTsuenTungLungIsland = Scraping.CoralSea.SamKaTsuenTungLungIsland.fetch
