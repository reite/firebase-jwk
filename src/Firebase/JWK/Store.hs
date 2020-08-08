{-# LANGUAGE OverloadedStrings #-}

module Firebase.JWK.Store (
    getCurrentKeys,
    createKeyStore,
    keyStoreKeys,
    KeyStore
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
import Firebase.JWK.Store.Internal 

-- | Get the current keys without using a store.
getCurrentKeys :: IO [JWK]
getCurrentKeys = googleKeysToJWKs . view responseBody <$> (asJSON =<< get firebaseKeysUrl)

-- * Exported API
-- --------------

-- | Get the current keys and put them into the store.
-- | This function expects the store to be empty.
fillKeyStore :: KeyStore -> IO ()
fillKeyStore = fillKeyStoreLogic defaultKeyStoreLogic
        
-- | Create a KeyStore. Will request the current keys before returning.
createKeyStore :: IO KeyStore
createKeyStore = createKeyStoreLogic defaultKeyStoreLogic

-- | Get the current keys. If they are expired get new ones, if not get from cache.
keyStoreKeys :: KeyStore -> IO [JWK]
keyStoreKeys = keyStoreKeysLogic defaultKeyStoreLogic
