{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Protolude                      ( Eq
                                                , Show
                                                , ($)
                                                , (<*>)
                                                , (<$>)
                                                , Generic
                                                )
import           Control.Lens.Combinators
import           Control.Lens.TH                (makeClassy)
import           Data.ByteString                ( ByteString )
import           Data.Aeson
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.Time.Clock                ( UTCTime )
import           Control.Concurrent.STM         ( TVar )
import           Crypto.Random.DRBG             ( HashDRBG
                                                , GenAutoReseed
                                                )

declareClassy [d|
  newtype Username = Username { usernameText :: Text }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)
  newtype Base64Content = Base64Content { base64ContentText :: Text }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype RawPassword = RawPassword { rawPasswordText :: Text }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype CreatedAt = CreatedAt { createdAtUTCTime :: UTCTime }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype LastUpdated = LastUpdated { lastUpdatedUTCTime :: UTCTime }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  data RegisterUser = RegisterUser
    { registerUserUsername :: Username
    , registerUserRawPassword :: RawPassword
    }
    deriving (Generic, Eq, Show)

  data LoginUser = LoginUser { loginUserUsername :: Username, loginUserRawPassword :: RawPassword }
    deriving (Eq, Show, Generic)

  newtype HashedPassword = HashedPassword { hashedPasswordText :: Text }

  newtype PasswordSalt = PasswordSalt { passwordSaltByteString :: ByteString }

  newtype TokenId = TokenId { tokenIdUUID :: UUID }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype UserId = UserId { userIdUUID :: UUID }
      deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype Expiry = Expiry { expiryUTCTime :: UTCTime }
      deriving (Eq, Show, Generic, ToJSON, FromJSON)

  data AccessToken = AccessToken
    { accessTokenTokenId ::  TokenId
    , accessTokenUserId :: UserId
    , accessTokenExpiry :: Expiry
    }
    deriving (Eq, Show, Generic)

  newtype SigningKey = SigningKey { signingKeyByteString :: ByteString }
    deriving (Eq, Show)

  data TaskStatus
    = New
    | NotNeeded
    | PendingResult
    | Complete
    deriving (Eq, Show, Generic)

  newtype SessionToken = SessionToken { sessionTokenBase64Content :: Base64Content }
    deriving (Eq, Show, Generic)

  |]

data AppState = AppState
  { _appStateCryptoRandomGen :: TVar (GenAutoReseed HashDRBG HashDRBG)
  , _appStateSigningKey :: SigningKey
  }
  deriving (Generic)
makeClassy ''AppState


instance FromJSON RegisterUser where
  parseJSON = withObject "loginUser" $ \o ->
    RegisterUser <$> o .: "username" <*> o .: "password"

instance FromJSON LoginUser where
  parseJSON = withObject "loginUser"
    $ \o -> LoginUser <$> o .: "username" <*> o .: "password"