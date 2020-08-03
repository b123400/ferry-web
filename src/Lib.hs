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
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (NominalDiffTime, getCurrentTime, addUTCTime)
import Data.Time.LocalTime (LocalTime(..), TimeZone, utcToLocalTime, hoursToTimeZone, midnight)
import Network.Wai
import Network.Wai.Handler.Warp
import Schedule.Calendar (HolidayCalendar)
import Scraping.Calendar (holidayCalendar)
import Servant
import System.Clock (TimeSpec(..))
import Timetable (Island, Route, limit, takeUntil, hongkongTimeZone)
import Timetable.Local (allIslandsAtTime, islandAtTime)
import Timetable.Raw (islandRaw, allIslandsRaw)
import Render.Lang (Localised(..), Lang, withLang)
import Render.Html (HTMLLucid)
import Render.Page.Index (Index(..))
import Render.Page.Detail (Detail(..))
import Render.Page.RawTimetable (RawTimetable(..))
import System.Environment (lookupEnv)
import Web.Cookie (SetCookie, defaultSetCookie, setCookieName, setCookieValue, setCookieExpires, setCookiePath)

import Debug.Trace

type WithLang a = Header "Accept-Language" Lang :> Header "Cookie" [SetCookie] :> a

type API = "static" :> Raw
      :<|> WithLang (Capture "island" Island :> QueryParam "count" Int :> QueryParam "date" Text :> Get '[JSON, HTMLLucid] (Localised Detail))
      :<|> WithLang (Capture "island" Island :> "raw" :> Get '[JSON, HTMLLucid] (Localised RawTimetable))
      :<|> "lang" :> Capture "lang" Lang :> Header "Referer" String :> Get '[HTMLLucid] NoContent
      :<|> "raws" :> Get '[JSON] [Route NominalDiffTime]
      :<|> "holidays" :> Get '[JSON] HolidayCalendar
      :<|> WithLang (QueryParam "count" Int :> Get '[JSON, HTMLLucid] (Localised Index))


startApp :: IO ()
startApp = do
    c <- newCache $ Just $ TimeSpec (60*60*3) 0 -- 3 hours
    port <- read <$> fromMaybe "8080" <$> lookupEnv "PORT"
    run port (app c)

app :: Cache String Dynamic -> Application
app c = serve api (server c)

api :: Proxy API
api = Proxy

server :: Cache String Dynamic -> Server API
server c = (serveDirectoryWebApp "static")
      :<|> (withLang $ detail c)
      :<|> (withLang $ rawDetail c)
      :<|> setLanguage
      :<|> raws c
      :<|> holidays c
      :<|> (withLang $ index c)

index :: Cache String Dynamic -> Lang -> Maybe Int -> Handler (Localised Index)
index cache lang count = liftIO $ do
    now <- getCurrentTime
    let lt = utcToLocalTime hongkongTimeZone now
        c = min 50 $ fromMaybe 4 count
        tomorrow = LocalTime (addDays 1 $ localDay lt) (localTimeOfDay lt)
    routes <- flip evalStateT cache $ runDyn $ runLocal $ allIslandsAtTime lt
    pure $ Localised lang $ Index lt (takeUntil tomorrow <$> limit c <$> routes)

detail :: Cache String Dynamic -> Lang -> Island -> Maybe Int -> Maybe Text -> Handler (Localised Detail)
detail cache lang island mcount mday = liftIO $ do
    now <- getCurrentTime
    let localNow = utcToLocalTime hongkongTimeZone now
        queriedDate@(LocalTime queriedDay _) =
            case (parseQueryParam <$> mday) of
                Just (Right day) -> LocalTime day midnight
                _ -> localNow
        count = min 50 $ fromMaybe 20 mcount
    route <- flip evalStateT cache $ runDyn $ runLocal $ islandAtTime island queriedDate
    pure $ Localised lang $ Detail localNow (limit count route) queriedDay count

rawDetail :: Cache String Dynamic -> Lang -> Island -> Handler (Localised RawTimetable)
rawDetail c lang island = liftIO $ Localised lang <$> RawTimetable <$> (flip evalStateT c $ runDyn $ runLocal $ islandRaw island)

setLanguage :: Lang -> Maybe String -> Handler NoContent
setLanguage lang from = do
    now <- liftIO getCurrentTime
    let cookieString = toHeader (defaultSetCookie
            { setCookieName = "lang"
            , setCookieValue = fromString $ show lang
            , setCookieExpires = Just $ addUTCTime (60*60*24*365) now
            , setCookiePath = Just "/"
            })
    throwError $ err303 { errHeaders =
        [("Location", fromString $ fromMaybe "/" from)
        ,("Set-Cookie", cookieString)
        ] }

raws :: Cache String Dynamic -> Handler [Route NominalDiffTime]
raws cache =
    liftIO $ flip evalStateT cache $ runDyn $ runLocal allIslandsRaw

holidays :: Cache String Dynamic -> Handler HolidayCalendar
holidays cache =
    liftIO $ flip evalStateT cache $ runDyn $ runLocal holidayCalendar
