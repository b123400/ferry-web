module Timetable.Raw where

import Control.Monad.Cache (MonadCache)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict as Map (Map)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (NominalDiffTime)

import Timetable (Route, Island(..), islands)
import Timetable.Class (HasTimetable(..))
import qualified Scraping.GovData.CentralCheungChau ()
import qualified Scraping.GovData.CentralMuiWo ()
import qualified Scraping.GovData.CentralPengChau ()
import qualified Scraping.GovData.CentralSokKwuWan ()
import qualified Scraping.GovData.CentralYungShueWan ()
import qualified Scraping.GovData.PengChauHeiLingChau ()
import qualified Scraping.GovData.AberdeenSokKwuWan ()
import qualified Scraping.GovData.CentralDiscoveryBay ()
import qualified Scraping.GovData.MaWanTsuenWan ()
import qualified Scraping.NWFF.NorthPointHungHom ()
import qualified Scraping.NWFF.NorthPointKowloonCity ()
import qualified Scraping.CoralSea.SaiWanHoKwunTong ()
import qualified Scraping.CoralSea.SaiWanHoSamKaTsuen ()
import qualified Scraping.CoralSea.SamKaTsuenTungLungIsland ()

type HasTimetables m =
    ( Monad m
    , HasTimetable m CentralCheungChau
    , HasTimetable m CentralMuiWo
    , HasTimetable m CentralPengChau
    , HasTimetable m CentralSokKwuWan
    , HasTimetable m CentralYungShueWan
    , HasTimetable m NorthPointHungHom
    , HasTimetable m NorthPointKowloonCity
    , HasTimetable m PengChauHeiLingChau
    , HasTimetable m AberdeenSokKwuWan
    , HasTimetable m CentralDiscoveryBay
    , HasTimetable m MaWanTsuenWan
    , HasTimetable m SaiWanHoKwunTong
    , HasTimetable m SaiWanHoSamKaTsuen
    , HasTimetable m SamKaTsuenTungLungIsland
    )

allIslandsRaw
    :: ( HasTimetables m )
    => m [Route NominalDiffTime]
allIslandsRaw = sequence $ islandRaw <$> islands

islandRaw
    :: ( HasTimetables m
       )
    => Island-> m (Route NominalDiffTime)
islandRaw CentralCheungChau = fetchTimetable (Proxy :: Proxy CentralCheungChau)
islandRaw CentralMuiWo = fetchTimetable (Proxy :: Proxy CentralMuiWo)
islandRaw CentralPengChau = fetchTimetable (Proxy :: Proxy CentralPengChau)
islandRaw CentralSokKwuWan = fetchTimetable (Proxy :: Proxy CentralSokKwuWan)
islandRaw CentralYungShueWan = fetchTimetable (Proxy :: Proxy CentralYungShueWan)
islandRaw NorthPointHungHom = fetchTimetable (Proxy :: Proxy NorthPointHungHom)
islandRaw NorthPointKowloonCity = fetchTimetable (Proxy :: Proxy NorthPointKowloonCity)
islandRaw PengChauHeiLingChau = fetchTimetable (Proxy :: Proxy PengChauHeiLingChau)
islandRaw AberdeenSokKwuWan = fetchTimetable (Proxy :: Proxy AberdeenSokKwuWan)
islandRaw CentralDiscoveryBay = fetchTimetable (Proxy :: Proxy CentralDiscoveryBay)
islandRaw MaWanTsuenWan = fetchTimetable (Proxy :: Proxy MaWanTsuenWan)
islandRaw SaiWanHoKwunTong = fetchTimetable (Proxy :: Proxy SaiWanHoKwunTong)
islandRaw SaiWanHoSamKaTsuen = fetchTimetable (Proxy :: Proxy SaiWanHoSamKaTsuen)
islandRaw SamKaTsuenTungLungIsland = fetchTimetable (Proxy :: Proxy SamKaTsuenTungLungIsland)
