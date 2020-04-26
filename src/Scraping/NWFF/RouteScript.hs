module Scraping.NWFF.RouteScript where

import Control.Monad.Catch (MonadCatch, handleAll)
import Control.Monad.Cache (MonadCache, withCache)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Map.Strict as Map (Map, fromList, (!?))
import Data.Maybe (mapMaybe)
import Network.HTTP.Conduit (simpleHttp)
import Text.Regex.TDFA ((=~))


regex :: String
regex = "<option value=\"([^\"]+)\" >([^<]+)<\\/option>"

-- e.g. {"North Point - Hung Hom" : "get_route.php?id=1c87d6ed-4ace-464c-b24e-4db2b83ce902&route_id=5&submenu_num=3"}
fetch :: (MonadCache m (Map String String), MonadIO m, MonadCatch m) => m (Map String String)
fetch = withCache "NWFF-RouteScript" $ do
    res <- simpleHttp "http://www.nwff.com.hk/route/schedule_select_route.php?lang=en"
    pure $ Map.fromList $ mapMaybe match $ res =~ regex
    where match (_ : link : route: []) = Just (toString route, toString link)
          match _ = Nothing

baseUrl :: String
baseUrl = "http://www.nwff.com.hk/route/"

linkOrDefault :: (MonadCache m (Map String String), MonadIO m, MonadCatch m) => String -> String -> m String
linkOrDefault key def = ((<>) baseUrl) <$> f
    where f = do
            routeMap <- handleAll (const $ pure mempty) fetch
            case (routeMap !? key) of
                Just n -> pure n
                Nothing -> pure def

-- Copied from official website 2020/03/26
centralCheungChau :: (MonadCache m (Map String String), MonadIO m, MonadCatch m) => m String
centralCheungChau = linkOrDefault "Central - Cheung Chau" "get_route.php?id=75852f2a-6be6-49bc-bb7a-e9d31f46b41e&route_id=8&submenu_num=3"

centralMuiWo :: (MonadCache m (Map String String), MonadIO m, MonadCatch m) => m String
centralMuiWo = linkOrDefault "Central - Mui Wo" "get_route.php?id=df056ac0-ac9e-4813-ba89-928c441dfec6&route_id=7&submenu_num=3"

interIslands :: (MonadCache m (Map String String), MonadIO m, MonadCatch m) => m String
interIslands = linkOrDefault "Inter Islands" "get_route.php?id=2e2c0154-902a-4c11-9405-f7743f6e6d2e&route_id=0&submenu_num=3"

northPointHungHom :: (MonadCache m (Map String String), MonadIO m, MonadCatch m) => m String
northPointHungHom = linkOrDefault "North Point - Hung Hom" "get_route.php?id=1c87d6ed-4ace-464c-b24e-4db2b83ce902&route_id=5&submenu_num=3"

northPointKowloonCity :: (MonadCache m (Map String String), MonadIO m, MonadCatch m) => m String
northPointKowloonCity = linkOrDefault "North Point - Kowloon City" "get_route.php?id=6662173e-a9ee-489b-aa36-8ce56dec0a6b&route_id=6&submenu_num=3"

