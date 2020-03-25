module Scraping.Calendar where

import Control.Monad.Cache (MonadCache, withCache)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Conduit (simpleHttp)
import Schedule.Calendar (HolidayCalendar, parseICal)


holidayCalendar :: (MonadIO m, MonadCache m HolidayCalendar) => m HolidayCalendar
holidayCalendar = withCache "HolidayCalendar" $ do
    res <- liftIO $ simpleHttp "https://www.1823.gov.hk/common/ical/en.ics"
    parseICal res
