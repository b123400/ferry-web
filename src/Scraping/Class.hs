module Scraping.Class where

import Control.Lens (Lens', lens, (^.), (.~), (&))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State (StateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Proxy (Proxy)
import Data.Time.Clock (NominalDiffTime)
import Text.XML.Cursor (Cursor)

import Timetable (Island(..), Route)


class Scrap (a :: Island) where
    route :: Proxy a -> Cursor -> Route NominalDiffTime

class HasCursor a where
    cursorL :: Lens' a (IO Cursor)

instance HasCursor (IO Cursor) where
    cursorL = lens id (const id)

shared :: forall a m o. MonadIO m => HasCursor a => (Cursor -> o) -> StateT a m o
shared fn = do
    state <- get
    cursor <- liftIO $ state ^. cursorL
    put $ state & cursorL .~ (pure cursor)
    pure $ fn cursor
