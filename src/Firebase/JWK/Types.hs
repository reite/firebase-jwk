

module Firebase.JWK.Types (
    module Firebase.JWK.Types
) where

import Data.Aeson
import Data.HashMap.Strict (HashMap, toList)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.X509 (Certificate, getCertificate)
import Data.X509.Memory (readSignedObjectFromMemory)


-- | A Certificate from google
newtype GoogleKey = GoogleKey { unGoogleKey :: Certificate }
    deriving (Show, Eq)

-- | Parse a certificate from Text
toCertificate :: Text -> Either String Certificate
toCertificate t = case readSignedObjectFromMemory . encodeUtf8 $ t of
    [c] -> return (getCertificate c)
    _   -> Left "Expected a single cerfiticate"

instance FromJSON GoogleKey where
    parseJSON = withText "GoogleKey" $ \s -> 
        fmap GoogleKey . either fail return . toCertificate $ s

-- | A map where the key-id is the key and a parsed JWK is the value
type GoogleKeys = HashMap Text GoogleKey





