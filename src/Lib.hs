{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Cache (Cache, newCache)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Data.Time.LocalTime (LocalTime(..), TimeZone, utcToLocalTime, hoursToTimeZone, midnight)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Clock (TimeSpec(..))
import Timetable (Island, Route, limit, hongkongTimeZone)
import Timetable.Local (allIslandsAtTime, addDiff, islandAtTime)
import Render.Html (HTMLLucid)
import Render.Page.Index (Index(..))
import Render.Page.Detail (Detail(..))

import Debug.Trace

import qualified Scraping.Islands.CheungChau (route)


type API = "static" :> Raw
      :<|> Capture "island" Island :> QueryParam "count" Int :> QueryParam "date" Text :> Get '[JSON, HTMLLucid] Detail
      :<|> QueryParam "count" Int :> Get '[JSON, HTMLLucid] Index

startApp :: IO ()
startApp = do
    c <- newCache $ Just $ TimeSpec (60*60*3) 0 -- 3 hours
    run 8080 (app c)

app :: Cache String (Route NominalDiffTime) -> Application
app c = serve api (server c)

api :: Proxy API
api = Proxy

server :: Cache String (Route NominalDiffTime) -> Server API
server c = (serveDirectoryWebApp "static")
      :<|> (detail c)
      :<|> (index c)

index :: Cache String (Route NominalDiffTime) -> Maybe Int -> Handler Index
index cache count = liftIO $ do
    now <- getCurrentTime
    let lt = utcToLocalTime hongkongTimeZone now
        c = min 50 $ fromMaybe 4 count
    routes <- allIslandsAtTime cache lt
    pure $ Index lt (limit c <$> routes)

detail :: Cache String (Route NominalDiffTime) -> Island -> Maybe Int -> Maybe Text -> Handler Detail
detail cache island mcount mday = liftIO $ do
    now <- getCurrentTime
    let localNow = utcToLocalTime hongkongTimeZone now
        queriedDate@(LocalTime queriedDay _) =
            case (parseQueryParam <$> mday) of
                Just (Right day) -> LocalTime day midnight
                _ -> localNow
        count = min 50 $ fromMaybe 20 mcount
    route <- islandAtTime cache island queriedDate
    pure $ Detail localNow (limit count route) queriedDay count
