
module Firebase.JWK.Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import qualified Data.ByteString.Lazy as LByteString

import Firebase.JWK.Types
import Firebase.JWK.Convert

test_typesFromJson :: TestTree
test_typesFromJson = testGroup "JWK"
    [ testCase "Can parse a set of keys" $ do
        keys <- getGoogleKeys
        case keys of 
            Right _ -> return ()
            Left er -> assertFailure er
    , testCase "Can convert GoogleKeys to JWK" $ do
        keys <- fmap googleKeysToJWKs <$> getGoogleKeys
        return ()
    ]
    where 
        getGoogleKeys :: IO (Either String GoogleKeys)
        getGoogleKeys = do
            bs <- LByteString.readFile "test/googlekeys.json"
            return $ eitherDecode bs
