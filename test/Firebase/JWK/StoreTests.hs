
module Firebase.JWK.StoreTests where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception (try, SomeException)
import Control.Concurrent.MVar
import Data.Time (getCurrentTime, UTCTime)
import System.Timeout
import Data.IORef

import Firebase.JWK.Store.Internal

-- | Helpers

trySomeError :: IO a -> IO (Either SomeException a)
trySomeError = try

-- | Logic for running keystore.

keyStoreLogicError :: KeyStoreLogic
keyStoreLogicError = KeyStoreLogic {
    requestKeys = \_ -> fail "Some error"
}

keyStoreLogicValid :: UTCTime -> KeyStoreLogic
keyStoreLogicValid expireTime = KeyStoreLogic {
    requestKeys = \_ -> return (expireTime, [])
}

keyStoreLogicValidSecondTime :: UTCTime -> IO KeyStoreLogic
keyStoreLogicValidSecondTime expireTime = do
    ref <- newIORef (1 :: Int)
    return KeyStoreLogic {
        requestKeys = \_ -> do
            count <- readIORef ref
            if count == 0 
                then return (expireTime, [])
                else do
                    writeIORef ref (count - 1)  
                    fail "Some error"
    }

assertKeyStoreNotEmpty :: KeyStore -> IO ()
assertKeyStoreNotEmpty ks = do
    let mvar = keyStoreContent ks
    value <- tryTakeMVar mvar
    case value of
        Nothing -> assertFailure "Key-store is empty, this will cause a deadlock."
        Just v  -> putMVar mvar v 

-- | Tests

test_keyStore :: TestTree
test_keyStore  = testGroup "KeyStoreLogic" 
    [ testCase "Can create keystore" $ do
        now <- getCurrentTime
        ks <- createKeyStoreLogic (keyStoreLogicValid now)
        return ()
        
    , testCase "Error on creating key store will cause error" $ do
        result <- trySomeError $ createKeyStoreLogic keyStoreLogicError
        case result of 
            Right _ -> assertFailure "Expected creting keystore to fail"
            Left  _ -> return ()

    

    , testCase "Error on second fill of keystore" $ do
        now <- getCurrentTime
        ks <- createKeyStoreLogic (keyStoreLogicValid now)
        result <- trySomeError $  keyStoreKeysLogic keyStoreLogicError ks
        case result of 
            Right _ -> assertFailure "Expected creating keystore to fail"
            Left  _ -> return ()

        return ()

    -- In version 0.1.0.0 this would case a deadlock.
    , testCase "Error on third fill of keystore" $ do
        now <- getCurrentTime
        ks <- createKeyStoreLogic (keyStoreLogicValid now)

        -- Second try
        trySomeError $  keyStoreKeysLogic keyStoreLogicError ks

        -- Third
        result <- timeout 100000 $ trySomeError $  keyStoreKeysLogic keyStoreLogicError ks

        assertKeyStoreNotEmpty ks

        case result of 
            Nothing -> assertFailure "Operation timed out, this means there is a deadlock."
            Just v  -> case v of
                Right _ -> assertFailure "Expected creating keystore to fail"
                Left  _ -> return ()

        return ()

    , testCase "Retries: Will succeed when first call is invalid but second is valid" $ do
        now <- getCurrentTime
        logic <- keyStoreLogicValidSecondTime now

        ks <- createKeyStoreLogic logic
        return ()
    ]