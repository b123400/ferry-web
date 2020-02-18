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
import Data.Time.LocalTime (LocalTime, utcToLocalTime, hoursToTimeZone)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Timetable (Route)
import Timetable.Local (allIslandsAtTime)
import Render.Html (HTMLLucid)

-- import Schedule.Calendar
-- import Schedule.Finder

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
    now <- getCurrentTime
    let hongkongTimeZone = hoursToTimeZone 9
        lt = utcToLocalTime hongkongTimeZone now
    allIslandsAtTime lt -- TODO: take 10 of ferries
