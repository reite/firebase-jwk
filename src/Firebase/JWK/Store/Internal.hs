{-# LANGUAGE OverloadedStrings #-}

module Firebase.JWK.Store.Internal where

import Control.Concurrent.MVar
import Control.Lens
import Data.Time
import Data.Time.Format
import Data.ByteString.Char8 (unpack)
import Network.Wreq
import Network.Wreq.Session (Session, newAPISession)
import qualified Network.Wreq.Session as Session
import Control.Exception (try, SomeException, throwIO)

import Firebase.JWK.Types
import Firebase.JWK.Convert

-- | Parsing time from 'expires' header.
timeParser :: String -> IO UTCTime 
timeParser = parseTimeM False defaultTimeLocale timeFormat
    where
        timeFormat = "%a, %d %b %_Y %T GMT"


-- | The url to get firebase keys
firebaseKeysUrl :: String
firebaseKeysUrl = "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"


-- | The KeyStore should be used when you want to use many keys over time. It caches 
-- | the keys so that you do not have to make a new request every time.
data KeyStore = KeyStore {
    keyStoreSession :: Session,
    keyStoreContent :: MVar (Either SomeException (UTCTime, [JWK]))
}



-- | Generalized testable logic

data KeyStoreLogic = KeyStoreLogic {
    requestKeys :: Session -> IO (UTCTime,  [JWK])
}

defaultRequestKeys :: Session -> IO (UTCTime,  [JWK])
defaultRequestKeys session = do
    response <- fmap googleKeysToJWKs <$> (asJSON =<< Session.get session firebaseKeysUrl)
    expireTime <- case response ^? responseHeader "expires" of
        Nothing  -> fail "Expected 'expires' header"
        (Just t) -> timeParser (unpack t)
    return (expireTime, response ^. responseBody)


defaultKeyStoreLogic :: KeyStoreLogic
defaultKeyStoreLogic = KeyStoreLogic {
    requestKeys = defaultRequestKeys
}


-- ** API
-- ------

fillKeyStoreLogic :: KeyStoreLogic -> KeyStore -> IO ()
fillKeyStoreLogic (KeyStoreLogic requestKeys) ks = do
    result <- try $ requestKeys (keyStoreSession ks)
    putMVar (keyStoreContent ks) result


-- | Create a KeyStore. Will request the current keys before returning.
--   We also check that initial fetch was successful so that we can return 
--   error early if something goes wrong.
createKeyStoreLogic :: KeyStoreLogic -> IO KeyStore
createKeyStoreLogic logic = do
    ks <- KeyStore <$> newAPISession <*> newEmptyMVar
    fillKeyStoreLogic logic ks

    let mvar = keyStoreContent ks
    value <- takeMVar mvar
    case value of
        Left e  -> throwIO e
        Right v -> do
            putMVar mvar value
            return ks


-- | Get the current keys. If they are expired get new ones, if not get from cache.
keyStoreKeysLogic :: KeyStoreLogic -> KeyStore -> IO [JWK]
keyStoreKeysLogic logic ks = do
    currentValue <- readMVar (keyStoreContent ks)
    case currentValue of 
        Left e -> throwIO e
        Right (expires, keys) -> do
            now <- getCurrentTime
            if now > expires
                then tryGetNewKeys
                else return keys
            where
                tryGetNewKeys = do
                    mv <- tryTakeMVar (keyStoreContent ks)
                    case mv of
                        Nothing  -> keyStoreKeysLogic logic ks
                        (Just _) -> fillKeyStoreLogic logic ks >> keyStoreKeysLogic logic ks



