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
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Data.Time.LocalTime (LocalTime)
import Network.HTTP.Conduit (simpleHttp)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, fromDocument)
import Text.XML hiding (parseLBS)
import Timetable (Route, localise, Direction(..))
import Render.Html (HTMLLucid)

import Schedule.Finder
import Debug.Trace

import qualified Scraping.Islands.CheungChau (route)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
      :<|> Get '[JSON, HTMLLucid] [Route LocalTime]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = (return users)
    :<|> index

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

index :: Handler [Route LocalTime]
index = liftIO $ do
    res <- simpleHttp "https://www.td.gov.hk/en/transport_in_hong_kong/public_transport/ferries/service_details/"
    let cursor = fromDocument $ parseLBS res
    routes <- findTimetables cursor
    now <- getCurrentTime
    let localised = fmap (localise now) <$> routes
    return localised

findTimetables :: Cursor -> IO [Route NominalDiffTime]
findTimetables cursor = do
    now <- getCurrentTime
    let route = Scraping.Islands.CheungChau.route cursor

    -- ferriesForRouteAtTime :: HolidayCalendar -> T.Route NominalDiffTime -> LocalTime -> T.Direction -> [T.Ferry LocalTime]
    let localised = localise now (60*60*8)
    let finderResult = take 200 $ ferriesForRouteAtTime () route localised ToIsland
    trace (show localised) $ pure ()
    trace (show finderResult) $ pure ()

    return [route]
