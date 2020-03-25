{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Cache (runLocal, runDyn)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Cache (Cache, newCache)
import Data.Dynamic (Dynamic)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime(..), TimeZone, utcToLocalTime, hoursToTimeZone, midnight)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Clock (TimeSpec(..))
import Timetable (Island, Route, limit, hongkongTimeZone)
import Timetable.Local (allIslandsAtTime, islandAtTime)
import Timetable.Raw (islandRaw)
import Render.Html (HTMLLucid)
import Render.Page.Index (Index(..))
import Render.Page.Detail (Detail(..))
import Render.Page.RawTimetable (RawTimetable(..))

import Debug.Trace


type API = "static" :> Raw
      :<|> Capture "island" Island :> QueryParam "count" Int :> QueryParam "date" Text :> Get '[JSON, HTMLLucid] Detail
      :<|> Capture "island" Island :> "raw" :> Get '[JSON, HTMLLucid] RawTimetable
      :<|> QueryParam "count" Int :> Get '[JSON, HTMLLucid] Index

startApp :: IO ()
startApp = do
    c <- newCache $ Just $ TimeSpec (60*60*3) 0 -- 3 hours
    run 8080 (app c)

app :: Cache String Dynamic -> Application
app c = serve api (server c)

api :: Proxy API
api = Proxy

server :: Cache String Dynamic -> Server API
server c = (serveDirectoryWebApp "static")
      :<|> (detail c)
      :<|> (rawDetail c)
      :<|> (index c)

index :: Cache String Dynamic -> Maybe Int -> Handler Index
index cache count = liftIO $ do
    now <- getCurrentTime
    let lt = utcToLocalTime hongkongTimeZone now
        c = min 50 $ fromMaybe 4 count
    routes <- flip evalStateT cache $ runDyn $ runLocal $ allIslandsAtTime lt
    pure $ Index lt (limit c <$> routes)

detail :: Cache String Dynamic -> Island -> Maybe Int -> Maybe Text -> Handler Detail
detail cache island mcount mday = liftIO $ do
    now <- getCurrentTime
    let localNow = utcToLocalTime hongkongTimeZone now
        queriedDate@(LocalTime queriedDay _) =
            case (parseQueryParam <$> mday) of
                Just (Right day) -> LocalTime day midnight
                _ -> localNow
        count = min 50 $ fromMaybe 20 mcount
    route <- flip evalStateT cache $ runDyn $ runLocal $ islandAtTime island queriedDate
    pure $ Detail localNow (limit count route) queriedDay count

rawDetail :: Cache String Dynamic -> Island -> Handler RawTimetable
rawDetail c island = liftIO $ RawTimetable <$> (flip evalStateT c $ runDyn $ runLocal $ islandRaw island)
