{-# LANGUAGE InstanceSigs #-}
module Control.Monad.Cache where

import Control.Lens (Lens', lens, view)
import Control.Monad.Catch (Exception, MonadCatch(..), MonadThrow(..), handleAll, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, gets)
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encodeFile)
import Data.ByteString.Lazy as BS (ByteString, readFile)
import Data.Cache as Cache (Cache, insert, lookup)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Typeable (Typeable)
import System.Directory (getModificationTime)

class Monad m => MonadCache m a where
    readCache :: String -> m (Maybe a)
    writeCache :: String -> a -> m ()

instance MonadCache IO a where
    readCache _ = pure Nothing
    writeCache _ _ = pure ()

readAndSaveParent :: (Monad (m n), MonadTrans m, MonadCache n a, MonadCache (m n) a) => String -> (m n) (Maybe a)
readAndSaveParent key = do
    v <- lift $ readCache key
    maybe (pure ()) (writeCache key) v
    pure v

instance (Has a (Cache String c), MonadIO m, MonadCache m c) => MonadCache (StateT a m) c where
    readCache :: String -> (StateT a m) (Maybe c)
    readCache key = do
        cache <- gets (view hasL)
        v <- liftIO $ Cache.lookup cache key
        maybe (readAndSaveParent key) (pure . Just) v

    writeCache key val = do
        cache <- gets (view hasL)
        liftIO $ Cache.insert cache key val
        lift $ writeCache key val

class Has a c where
    hasL :: Lens' a c

instance Has a a where
    hasL = lens id (const id)

withCache :: MonadCache m a => String -> m a -> m a
withCache key op = do
    val <- readCache key
    case val of
        Just v -> pure v
        Nothing -> do
            v <- op
            writeCache key v
            pure v

newtype Dyn m a = Dyn { runDyn :: m a }

instance Functor m => Functor (Dyn m) where
    fmap fn (Dyn op) = Dyn $ fmap fn op

instance Applicative m => Applicative (Dyn m) where
    (<*>) (Dyn fn) (Dyn op) = Dyn $ fn <*> op
    pure = Dyn . pure

instance Monad m => Monad (Dyn m) where
    (>>=) (Dyn op) fn = Dyn $ op >>= \a -> let (Dyn m) = fn a in m

instance MonadThrow m => MonadThrow (Dyn m) where
    throwM e = Dyn $ throwM e

instance MonadCatch m => MonadCatch (Dyn m) where
    catch (Dyn op) fn = Dyn $ catch op (\e-> let (Dyn m) = fn e in m)

instance MonadIO m => MonadIO (Dyn m) where
    liftIO io = Dyn $ liftIO io

instance MonadTrans Dyn where
    lift = Dyn

instance (Has a (Cache String Dynamic), MonadIO m, MonadCache m Dynamic, Typeable c) => MonadCache (Dyn (StateT a m)) c where
    readCache key = do
        v <- lift $ readCache key
        pure $ fromDynamic =<< v

    writeCache key v = lift $ writeCache key (toDyn v)

type Serialisable a = (ToJSON a, FromJSON a)
type MonadDynCache m = MonadCache m Dynamic

newtype Local m a = Local { runLocal :: m a }

instance Functor m => Functor (Local m) where
    fmap fn (Local op) = Local $ fmap fn op

instance Applicative m => Applicative (Local m) where
    (<*>) (Local fn) (Local op) = Local $ fn <*> op
    pure = Local . pure

instance Monad m => Monad (Local m) where
    (>>=) (Local op) fn = Local $ op >>= \a -> let (Local m) = fn a in m

instance MonadThrow m => MonadThrow (Local m) where
    throwM e = Local $ throwM e

instance MonadCatch m => MonadCatch (Local m) where
    catch (Local op) fn = Local $ catch op (\e-> let (Local m) = fn e in m)

instance MonadIO m => MonadIO (Local m) where
    liftIO io = Local $ liftIO io

instance MonadTrans Local where
    lift = Local

instance (MonadIO m, MonadCatch m, MonadCache m a, Serialisable a) => MonadCache (Local m) a where
    readCache filePath = do
        v <- handleAll (const $ pure Nothing) readLocalCache
        maybe (readAndSaveParent filePath) (pure . Just) v
        where
            readLocalCache = do
                lastModified <- liftIO $ getModificationTime filePath
                now <- liftIO $ getCurrentTime
                if diffUTCTime now lastModified > 60*60*24*7
                    then pure Nothing
                    else do
                        c <- liftIO $ BS.readFile filePath
                        case eitherDecode c of
                            Left a -> pure Nothing
                            Right a -> pure $ Just a
    writeCache filePath val = do
        liftIO $ encodeFile filePath val
        lift $ writeCache filePath val

instance ToJSON ByteString where
    toJSON = toJSON . decodeUtf8

instance FromJSON ByteString where
    parseJSON = fmap encodeUtf8 . parseJSON
