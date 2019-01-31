{-# LANGUAGE OverloadedStrings #-}
module Handlers
    ( Handler
    , createUserHandler
    , retrieveUserHandler
    , updateUserHandler
    , responseOk
    , responseNotFound
    )
where

import           Network.Wai
import qualified          Network.HTTP.Types as HTTP
import           Models
import           Serializers
import           Database
import MonadHandler
import           Data.Aeson
import           Data.Text
import           Data.Maybe
import           Text.Read
import           Control.Exception              ( catch
                                                , SomeException
                                                )
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Config                        as C

responseOk :: Applicative m => LBS.ByteString -> m Response
responseOk b = pure $ responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] b

responseNotFound :: Applicative m => m Response
responseNotFound = pure $ responseLBS HTTP.status404
                                      [(HTTP.hContentType, "text/plain")]
                                      "Resource not found"

createUserHandler :: Handler
createUserHandler = do
    req <- asks hRequest
    body <- liftIO $ strictRequestBody req
    either errorValidation successValidation (eitherDecode body)
  where
    errorValidation _ = responseNotFound
    successValidation d = do
        conn  <- asks hConnection
        query <- liftIO $ insertUser
            conn
            defaultUser { userFirstName = createUserRawFirstName d
                        , userLastName = createUserRawLastName d
                        , userIsAdmin = fromMaybe False (createUserRawIsAdmin d)
                        }
        either errorResponse successResponse query
      where
        errorResponse :: SomeException -> MonadHandler Response
        errorResponse _ = responseNotFound
        successResponse d = responseOk $ encode (serializeUserSuccess d)

retrieveUserHandler :: Handler
retrieveUserHandler = do
    pks <- asks hPks
    case lookup "pk" pks >>= (readMaybe . unpack) of
        Nothing -> responseNotFound
        Just pk -> do
            conn <- asks hConnection
            res  <- liftIO $ selectUser conn pk
            either errorResponse successResponse res
  where
    errorResponse :: SomeException -> MonadHandler Response
    errorResponse _ = responseNotFound
    successResponse Nothing = responseNotFound
    successResponse (Just user) =
        responseOk $ encode (serializeUserSuccess user)

updateUserHandler :: Handler
updateUserHandler = responseOk "Update User Route!"
