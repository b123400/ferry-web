{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy.Char8 (pack)
import Data.Cache (Cache, newCache)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Data.Time.LocalTime (LocalTime, TimeZone, utcToLocalTime, hoursToTimeZone)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Clock (TimeSpec(..))
import Timetable (Island, Route, limit)
import Timetable.Local (allIslandsAtTime, addDiff, islandAtTime)
import Render.Html (HTMLLucid)

import Debug.Trace

import qualified Scraping.Islands.CheungChau (route)


type API = "static" :> Raw
      :<|> Capture "island" Island :> QueryParam "count" Int :> Get '[JSON, HTMLLucid] (Route (LocalTime, NominalDiffTime))
      :<|> QueryParam "count" Int :> Get '[JSON, HTMLLucid] [Route (LocalTime, NominalDiffTime)]

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

index :: Cache String (Route NominalDiffTime) -> Maybe Int -> Handler [Route (LocalTime, NominalDiffTime)]
index cache count = liftIO $ do
    now <- getCurrentTime
    let lt = utcToLocalTime hongkongTimeZone now
        c = min 50 $ fromMaybe 4 count
    routes <- allIslandsAtTime cache lt
    let withDiff = fmap (addDiff lt) <$> routes
    pure $ limit c <$> withDiff

detail :: Cache String (Route NominalDiffTime) -> Island -> Maybe Int -> Handler (Route (LocalTime, NominalDiffTime))
detail cache island mcount = liftIO $ do
    now <- getCurrentTime
    let lt = utcToLocalTime hongkongTimeZone now
        c = min 50 $ fromMaybe 10 mcount
    route <- islandAtTime cache island lt
    let withDiff = addDiff lt <$> route
    pure $ limit c withDiff

hongkongTimeZone :: TimeZone
hongkongTimeZone = hoursToTimeZone 8
