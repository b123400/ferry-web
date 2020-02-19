module Data.Cache where

import Prelude hiding (readFile)

import Control.Monad.Catch (Exception, MonadCatch, handleAll, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encodeFile)
import Data.ByteString.Lazy (readFile)
import Data.Proxy (Proxy(..))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (getModificationTime)

data CacheException = CannotDecodeCache String
                    | CacheExpired
    deriving Show

instance Exception CacheException

withCacheFile :: ToJSON a => FromJSON a => MonadIO m => MonadCatch m => FilePath -> m a -> m a
withCacheFile filePath op = handleAll run readCache
    where
        run _ = do
            r <- op
            writeCache r
            pure r

        readCache = do
            lastModified <- liftIO $ getModificationTime filePath
            now <- liftIO $ getCurrentTime
            if diffUTCTime now lastModified > 60*60*24*7
                then throwM CacheExpired
                else pure ()
            c <- liftIO $ readFile filePath
            case eitherDecode c of
                Left a -> throwM $ CannotDecodeCache a
                Right a -> pure a

        writeCache = liftIO . encodeFile filePath

withCache :: ToJSON a => FromJSON a => MonadIO m => MonadCatch m => Cache i => Proxy i -> m a -> m a
withCache p = withCacheFile $ cacheFilename p

withCache' :: forall a m. ToJSON a => FromJSON a => Cache a => MonadIO m => MonadCatch m => m a -> m a
withCache' = withCache (Proxy :: Proxy a)

class Cache (a :: k) where
    cacheFilename :: Proxy a -> FilePath
