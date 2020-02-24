module Scraping.Calendar where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.LocalCache (withLocalCache')
import Schedule.Calendar (HolidayCalendar, fetchHolidayICal)

holidayCalendar :: MonadCatch m => MonadIO m => m HolidayCalendar
holidayCalendar = withLocalCache' $ liftIO fetchHolidayICal
