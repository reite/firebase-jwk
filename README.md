# firebase-jwk

To verify a JWT ID tokens from the firebase-auth api you need a key from google.
This library allows converting keys in the PEM format that google provider to JWK.
It also provides a way to cache keys for long running processes.

To get the keys once use `getCurrentKeys :: IO [JWK]`

Use the keystore if you need to get keys more often. The keystore will cache the keys and get new ones when they expire.

```haskell
main :: IO ()
main = do
    keyStore <- createKeyStore
    keys <- keyStoreKeys
    print keys
```

Below is a example of how to use the `jose` package to validate JWTs from firebase.

```haskell

import Data.ByteString.Lazy (ByteString)
import Crypto.JWT
import Control.Monad.Except

import Firebase.JWK.Store

verifyFirebaseJWT :: ByteString -> IO (Either JWTError ClaimsSet)
verifyFirebaseJWT tok = runExceptT $ do
    jwt <- decodeCompact tok
    jwkSet <- liftIO $ JWKSet <$> getCurrentKeys
    let config = defaultJWTValidationSettings (== "<firebase project id>")
    verifyClaims config jwkSet jwt
```