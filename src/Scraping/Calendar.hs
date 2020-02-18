module Scraping.Calendar where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Cache (withCache')
import Schedule.Calendar (HolidayCalendar, fetchHolidayICal)

holidayCalendar :: MonadCatch m => MonadIO m => m HolidayCalendar
holidayCalendar = withCache' $ liftIO fetchHolidayICal
