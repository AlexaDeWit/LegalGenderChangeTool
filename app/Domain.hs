module Domain where

import           Protolude
import           Control.Lens
import           Types
import           Crypto                         ( hashPassword )
import           Schema
import           Data.UUID                      ( UUID )
import           Data.Time.LocalTime            ( utcToLocalTime
                                                , utc
                                                )
import           Data.Time.Clock                ( getCurrentTime
                                                )
import           System.Random                  ( randomIO )
import           Crypto.Argon2                  ( Argon2Status )

newUser :: RegisterUser -> PasswordSalt -> ExceptT Argon2Status IO User
newUser (RegisterUser name rawPass) (PasswordSalt salt) = do
  uuid       <- lift (randomIO :: IO UUID)
  hashedPass <- ExceptT $ pure $ hashPassword salt rawPass
  now        <- lift $ utcToLocalTime utc <$> getCurrentTime
  pure $ User uuid
              (name ^. usernameText)
              (hashedPass ^. hashedPasswordText)
              now
              now