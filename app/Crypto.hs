module Crypto where

import           Protolude                      ( ($)
                                                , (<$>)
                                                , Either(..)
                                                , fmap
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Text.Short                ( fromText
                                                , toText
                                                )
import           Data.Text.Encoding             ( encodeUtf8
                                                )
import           Crypto.Argon2                  ( verifyEncoded
                                                , hashEncoded
                                                , defaultHashOptions
                                                , Argon2Status
                                                )
import           Types                          ( RawPassword(..)
                                                , HashedPassword(..)
                                                )

hashPassword :: ByteString -> RawPassword -> Either Argon2Status HashedPassword
hashPassword salt (RawPassword pass) =
  fmap HashedPassword
    $   toText
    <$> hashEncoded defaultHashOptions (encodeUtf8 pass) salt

verifyPassword :: HashedPassword -> RawPassword -> Argon2Status
verifyPassword (HashedPassword pass) (RawPassword raw) =
  verifyEncoded (fromText pass) (encodeUtf8 raw)
