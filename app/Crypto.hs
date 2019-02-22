module Crypto where

import           Protolude                      ( ($)
                                                , (<$>)
                                                , Either(..)
                                                , fmap
                                                , note
                                                )
import           Data.Aeson                     ( ToJSON
                                                , encode
                                                , FromJSON
                                                , decode
                                                )
import           Control.Arrow                  ( (<<<) )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Lazy           ( toStrict
                                                , fromStrict
                                                )
import           Data.Text.Short                ( fromText
                                                , toText
                                                )
import           Data.Text                      ( Text )
import           Jose.Jwa                       ( JwsAlg(HS512) )
import           Jose.Jwt                       ( JwtError
                                                , unJwt
                                                , JwtError(..)
                                                )
import           Jose.Jws                       ( hmacEncode
                                                , hmacDecode
                                                )
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Crypto.Argon2                  ( verifyEncoded
                                                , hashEncoded
                                                , defaultHashOptions
                                                , Argon2Status
                                                )
import           Types

hashPassword :: ByteString -> RawPassword -> Either Argon2Status HashedPassword
hashPassword salt (RawPassword pass) =
  fmap HashedPassword
    $   toText
    <$> hashEncoded defaultHashOptions (encodeUtf8 pass) salt

verifyPassword :: HashedPassword -> RawPassword -> Argon2Status
verifyPassword (HashedPassword pass) (RawPassword raw) =
  verifyEncoded (fromText pass) (encodeUtf8 raw)

signJwt
  :: ToJSON a => SigningKey -> a -> Either JwtError ConcatenatedSessionToken
signJwt (SigningKey k) a =
  (ConcatenatedSessionToken <<< decodeUtf8 <<< unJwt)
    <$> hmacEncode HS512 k (toStrict $ encode a)

verifyJwt :: FromJSON a => SigningKey -> Text -> Either JwtError a
verifyJwt (SigningKey k) candidate = do
  (_, message) <- hmacDecode k (encodeUtf8 candidate)
  note BadClaims $ decode $ fromStrict message
