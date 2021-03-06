module Queries where

import           Prelude
import           Control.Lens
import           Schema
import           Types
import           Data.ByteString                ( ByteString )
import           Data.String.QQ
import           Hasql.Statement
import qualified Hasql.Encoders                as HE
import qualified Hasql.Decoders                as HD

decodeTaskStatus :: HD.Value TaskStatus
decodeTaskStatus = HD.enum f where
    f t = case t of
        "New"           -> Just New
        "NotNeeded"     -> Just NotNeeded
        "PendingResult" -> Just PendingResult
        "Complete"      -> Just Complete
        _               -> Nothing

encodeTaskStatus :: HE.Value TaskStatus
encodeTaskStatus = HE.enum f  where
    f ts = case ts of
        New           -> "New"
        NotNeeded     -> "NotNeeded"
        PendingResult -> "PendingResult"
        Complete      -> "Complete"

findUserByUsername :: Statement Username (Maybe User)
findUserByUsername = Statement sqlS encoder decoder True
    where
        sqlS :: ByteString
        sqlS =  
            [s|
                SELECT
                    "userUuid",
                    "userUsername",
                    "userHashedPassword",
                    "userLastUpdated",
                    "userCreatedAt"
                FROM
                    "users"
                WHERE
                    "userUsername"=$1
            |]
        encoder = HE.param usernameValue
        decoder = 
            HD.rowMaybe
            $ User
            <$> HD.column HD.uuid
            <*> HD.column HD.text
            <*> HD.column HD.text
            <*> HD.column HD.timestamp
            <*> HD.column HD.timestamp

insertUser :: Statement User ()
insertUser = Statement sqlS encoder decoder True
    where
        sqlS :: ByteString
        sqlS =
            [s|
                INSERT INTO "users"
                (
                    "userUuid",
                    "userUsername",
                    "userHashedPassword",
                    "userLastUpdated",
                    "userCreatedAt"
                )
                VALUES (
                  $1, $2, $3, $4, $5, $6
                )
            |]
        encoder =
            contramap _userUuid (HE.param HE.uuid) <>
            contramap _userUsername (HE.param HE.text) <>
            contramap _userHashedPassword (HE.param HE.text) <>
            contramap _userLastUpdated (HE.param HE.timestamp) <>
            contramap _userCreatedAt (HE.param HE.timestamp)
        decoder = HD.unit

usernameValue :: HE.Value Username
usernameValue = contramap (^. usernameText) HE.text