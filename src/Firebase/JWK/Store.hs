{-# LANGUAGE OverloadedStrings #-}

module Firebase.JWK.Store (
    getCurrentKeys,
    createKeyStore,
    keyStoreKeys
) where

import Control.Concurrent.MVar
import Control.Lens
import Data.Time
import Data.Time.Format
import Data.ByteString.Char8 (unpack)
import Network.Wreq
import Network.Wreq.Session (Session, newAPISession)
import qualified Network.Wreq.Session as Session

import Firebase.JWK.Types
import Firebase.JWK.Convert



-- | The url to get firebase keys
firebaseKeysUrl :: String
firebaseKeysUrl = "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"


-- | Get the current keys without using a store.
getCurrentKeys :: IO [JWK]
getCurrentKeys = googleKeysToJWKs . view responseBody <$> (asJSON =<< get firebaseKeysUrl)



-- | The KeyStore should be used when you want to use many keys over time. It caches 
-- | the keys so that you do not have to make a new request every time.
data KeyStore = KeyStore {
    keyStoreSession :: Session,
    keyStoreContent :: MVar (UTCTime, [JWK])
}

fillKeyStore :: KeyStore -> IO ()
fillKeyStore ks = do
    response <- fmap googleKeysToJWKs <$> (asJSON =<< Session.get (keyStoreSession ks) firebaseKeysUrl)
    expireTime <- case response ^? responseHeader "expires" of
        Nothing  -> fail "Expected 'expires' header"
        (Just t) -> timeParser (unpack t)

    putMVar (keyStoreContent ks) (expireTime, response ^. responseBody)
    where
        timeParser :: String -> IO UTCTime 
        timeParser = parseTimeM False defaultTimeLocale timeFormat

        timeFormat = "%a, %d %b %_Y %T GMT"


createKeyStore :: IO KeyStore
createKeyStore = do
    ks <- KeyStore <$> newAPISession <*> newEmptyMVar
    fillKeyStore ks
    return ks

keyStoreKeys :: KeyStore -> IO [JWK]
keyStoreKeys ks = do
    (expires, keys) <- readMVar (keyStoreContent ks)
    now <- getCurrentTime
    if now > expires
        then tryGetNewKeys
        else return keys
    where
        tryGetNewKeys = do
            mv <- tryTakeMVar (keyStoreContent ks)
            case mv of
                Nothing  -> keyStoreKeys ks
                (Just _) -> fillKeyStore ks >> keyStoreKeys ks

