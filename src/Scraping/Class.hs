module Scraping.Class where

import Control.Monad.Trans.State (StateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Proxy (Proxy)
import Data.Time.Clock (NominalDiffTime)
import Text.XML.Cursor (Cursor)

import Timetable (Island(..), Route)


class Scrap (a :: Island) where
    route :: Proxy a -> Cursor -> Route NominalDiffTime


shared :: Monad m => (i -> o) -> StateT (m i) m o
shared fn = do
    mi <- get
    i <- lift mi
    put $ pure i
    pure $ fn i
