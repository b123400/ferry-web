module Timetable.Raw where

import Control.Monad.Cache (MonadCache)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict as Map (Map, fromList)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (NominalDiffTime)

import Timetable (Route, Island(..), islands)
import Timetable.Metadata (Metadata)
import Timetable.Class (HasTimetable(..), HasMetadata(..))
import qualified Scraping.GovData.CentralCheungChau.Timetable ()
import qualified Scraping.GovData.CentralCheungChau.Metadata ()
import qualified Scraping.GovData.CentralMuiWo.Timetable ()
import qualified Scraping.GovData.CentralMuiWo.Metadata ()
import qualified Scraping.GovData.CentralPengChau.Timetable ()
import qualified Scraping.GovData.CentralPengChau.Metadata ()
import qualified Scraping.GovData.CentralSokKwuWan.Timetable ()
import qualified Scraping.GovData.CentralSokKwuWan.Metadata ()
import qualified Scraping.GovData.CentralYungShueWan.Timetable ()
import qualified Scraping.GovData.CentralYungShueWan.Metadata ()
import qualified Scraping.GovData.CentralMaWan.Timetable ()
import qualified Scraping.GovData.CentralMaWan.Metadata ()
import qualified Scraping.GovData.PengChauHeiLingChau.Timetable ()
import qualified Scraping.GovData.PengChauHeiLingChau.Metadata ()
import qualified Scraping.GovData.AberdeenSokKwuWan.Timetable ()
import qualified Scraping.GovData.AberdeenSokKwuWan.Metadata ()
import qualified Scraping.GovData.CentralDiscoveryBay.Timetable ()
import qualified Scraping.GovData.MaWanTsuenWan.Timetable ()
import qualified Scraping.GovData.MaWanTsuenWan.Metadata ()
import qualified Scraping.Gov.NorthPointHungHom.Timetable ()
import qualified Scraping.GovData.NorthPointHungHom.Metadata ()
import qualified Scraping.Gov.NorthPointKowloonCity.Timetable ()
import qualified Scraping.GovData.NorthPointKowloonCity.Metadata ()
import qualified Scraping.CoralSea.SaiWanHoKwunTong.Timetable ()
import qualified Scraping.CoralSea.SaiWanHoKwunTong.Metadata ()
import qualified Scraping.CoralSea.SaiWanHoSamKaTsuen.Timetable ()
import qualified Scraping.CoralSea.SaiWanHoSamKaTsuen.Metadata ()
import qualified Scraping.CoralSea.SamKaTsuenTungLungIsland.Timetable ()

import qualified Scraping.Gov.CentralMuiWo as MuiWo

type HasTimetables m =
    ( Monad m

    -- We need these only for the old MuiWo implementation
    , MonadIO m
    , MonadCache m ByteString
    , MonadCache m (Route NominalDiffTime)

    , HasTimetable m CentralCheungChau
    , HasTimetable m CentralMuiWo
    , HasTimetable m CentralPengChau
    , HasTimetable m CentralSokKwuWan
    , HasTimetable m CentralYungShueWan
    , HasTimetable m CentralMaWan
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
    => Island -> m (Route NominalDiffTime)
islandRaw CentralCheungChau = fetchTimetable (Proxy @CentralCheungChau)
-- islandRaw CentralMuiWo = fetchTimetable (Proxy @CentralMuiWo)
islandRaw CentralMuiWo = MuiWo.fetch
islandRaw CentralPengChau = fetchTimetable (Proxy @CentralPengChau)
islandRaw CentralSokKwuWan = fetchTimetable (Proxy @CentralSokKwuWan)
islandRaw CentralYungShueWan = fetchTimetable (Proxy @CentralYungShueWan)
islandRaw CentralMaWan = fetchTimetable (Proxy @CentralMaWan)
islandRaw NorthPointHungHom = fetchTimetable (Proxy @NorthPointHungHom)
islandRaw NorthPointKowloonCity = fetchTimetable (Proxy @NorthPointKowloonCity)
islandRaw PengChauHeiLingChau = fetchTimetable (Proxy @PengChauHeiLingChau)
islandRaw AberdeenSokKwuWan = fetchTimetable (Proxy @AberdeenSokKwuWan)
islandRaw CentralDiscoveryBay = fetchTimetable (Proxy @CentralDiscoveryBay)
islandRaw MaWanTsuenWan = fetchTimetable (Proxy @MaWanTsuenWan)
islandRaw SaiWanHoKwunTong = fetchTimetable (Proxy @SaiWanHoKwunTong)
islandRaw SaiWanHoSamKaTsuen = fetchTimetable (Proxy @SaiWanHoSamKaTsuen)
islandRaw SamKaTsuenTungLungIsland = fetchTimetable (Proxy @SamKaTsuenTungLungIsland)

type HasMetadatas m =
    ( Monad m
    , HasMetadata m CentralCheungChau
    , HasMetadata m CentralMuiWo
    , HasMetadata m CentralPengChau
    , HasMetadata m CentralSokKwuWan
    , HasMetadata m CentralYungShueWan
    , HasMetadata m CentralMaWan
    , HasMetadata m NorthPointHungHom
    , HasMetadata m NorthPointKowloonCity
    , HasMetadata m PengChauHeiLingChau
    , HasMetadata m AberdeenSokKwuWan
    -- , HasMetadata m CentralDiscoveryBay
    , HasMetadata m MaWanTsuenWan
    , HasMetadata m SaiWanHoKwunTong
    , HasMetadata m SaiWanHoSamKaTsuen
    -- , HasMetadata m SamKaTsuenTungLungIsland
    )

metadatasRaw :: (HasMetadatas m)=> m (Map Island Metadata)
metadatasRaw = Map.fromList <$> mapM (\i-> ((,) i) <$> metadataRaw i) islands
    where islands = [ CentralCheungChau
                    , CentralMuiWo
                    , CentralPengChau
                    , CentralSokKwuWan
                    , CentralYungShueWan
                    , CentralMaWan
                    , NorthPointHungHom
                    , NorthPointKowloonCity
                    , PengChauHeiLingChau
                    , AberdeenSokKwuWan
                    , MaWanTsuenWan
                    , SaiWanHoKwunTong
                    , SaiWanHoSamKaTsuen
                    ]

metadataRaw :: (HasMetadatas m)=> Island -> m Metadata
metadataRaw CentralCheungChau = fetchMetadata (Proxy @CentralCheungChau)
metadataRaw CentralMuiWo = fetchMetadata (Proxy @CentralMuiWo)
metadataRaw CentralPengChau = fetchMetadata (Proxy @CentralPengChau)
metadataRaw CentralSokKwuWan = fetchMetadata (Proxy @CentralSokKwuWan)
metadataRaw CentralYungShueWan = fetchMetadata (Proxy @CentralYungShueWan)
metadataRaw CentralMaWan = fetchMetadata (Proxy @CentralMaWan)
metadataRaw NorthPointHungHom = fetchMetadata (Proxy @NorthPointHungHom)
metadataRaw NorthPointKowloonCity = fetchMetadata (Proxy @NorthPointKowloonCity)
metadataRaw PengChauHeiLingChau = fetchMetadata (Proxy @PengChauHeiLingChau)
metadataRaw AberdeenSokKwuWan = fetchMetadata (Proxy @AberdeenSokKwuWan)
metadataRaw MaWanTsuenWan = fetchMetadata (Proxy @MaWanTsuenWan)
metadataRaw SaiWanHoKwunTong = fetchMetadata (Proxy @SaiWanHoKwunTong)
metadataRaw SaiWanHoSamKaTsuen = fetchMetadata (Proxy @SaiWanHoSamKaTsuen)
metadataRaw _ = error "not yet"
