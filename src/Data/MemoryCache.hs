module Data.MemoryCache where

import Control.Lens (Lens', lens, view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State (StateT, gets)
import Control.Monad.Trans.Class (lift)
import Data.Cache (Cache, fetchWithCache)
import Data.Proxy (Proxy(..))
import qualified Data.LocalCache as LC

class HasCache a o | a -> o where
    cacheL :: Lens' a (Cache String o)

instance HasCache (Cache String o) o where
    cacheL = lens id (const id)

withKeyedMemoryCache :: HasCache a o => MonadIO m => String -> StateT a m o -> StateT a m o
withKeyedMemoryCache key op = do
    cache <- gets (view cacheL)
    fetchWithCache cache key (const op)

withMemoryCache :: LC.Cache i => HasCache a o => MonadIO m => Proxy i -> StateT a m o -> StateT a m o
withMemoryCache p = withKeyedMemoryCache (LC.cacheFilename p)

withMemoryCache' :: forall o a m. LC.Cache o => HasCache a o => MonadIO m => StateT a m o -> StateT a m o
withMemoryCache' = withMemoryCache (Proxy :: Proxy o)
