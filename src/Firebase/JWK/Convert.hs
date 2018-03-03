
module Firebase.JWK.Convert (
    module Firebase.JWK.Convert,

    JWK
) where

import Control.Lens
import Crypto.JOSE.JWK
import qualified Crypto.JOSE.Types as Types
import Crypto.PubKey.RSA.Types
import Crypto.JOSE.JWA.JWS
import Data.HashMap.Strict (HashMap, toList)
import Data.X509

import Firebase.JWK.Types



pubKeyToParams ::  PublicKey -> RSAKeyParameters
pubKeyToParams (PublicKey s n e) = RSAKeyParameters (Types.SizedBase64Integer s n) (Types.Base64Integer e) Nothing

certificateToKeyParams :: Certificate -> RSAKeyParameters
certificateToKeyParams = pubKeyToParams . toPubKey . certPubKey
    where
        toPubKey (PubKeyRSA  pb) = pb

paramsToJWK :: RSAKeyParameters -> JWK
paramsToJWK = fromKeyMaterial . RSAKeyMaterial 



certificateToJWK :: Certificate -> JWK
certificateToJWK = paramsToJWK . certificateToKeyParams


-- |Convert a set of google keys to JWKs. 
googleKeysToJWKs :: GoogleKeys -> [JWK]
googleKeysToJWKs = map convert . toList
    where
        convert (kid, gkey) = certificateToJWK (unGoogleKey gkey)
            & jwkKid .~ Just kid
            & jwkAlg .~ Just (JWSAlg RS256)
            & jwkUse .~ Just Sig