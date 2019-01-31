{-# LANGUAGE OverloadedStrings #-}
module API.Handlers
    ( Handler
    , createUserHandler
    , retrieveUserHandler
    , updateUserHandler
    , responseOk
    , responseNotFound
    )
where

import           Network.Wai
import qualified Network.HTTP.Types            as HTTP
import           Data.Aeson
import           Data.Text
import           Data.Maybe
import           Text.Read
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Control.Exception             as EX
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as LBS

import           Monad.Handler
import           Models.User
import qualified Core.Database                 as DB
import qualified Core.Config                   as C

responseOk :: Applicative m => LBS.ByteString -> m Response
responseOk b =
    pure $ responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] b

responseNotFound :: Applicative m => m Response
responseNotFound = pure $ responseLBS HTTP.status404
                                      [(HTTP.hContentType, "text/plain")]
                                      "Resource not found"

createUserHandler :: Handler
createUserHandler = do
    req  <- asks hRequest
    body <- liftIO $ strictRequestBody req
    either errorValidation successValidation (eitherDecode body)
  where
    errorValidation _ = responseNotFound
    successValidation d = do
        conn  <- asks hConnection
        query <- liftIO $ DB.insertUser
            conn
            defaultUser { userFirstName = userRawFirstName d
                        , userLastName  = userRawLastName d
                        , userIsAdmin   = fromMaybe False (userRawIsAdmin d)
                        }
        either errorResponse successResponse query
      where
        errorResponse :: EX.SomeException -> MonadHandler Response
        errorResponse _ = responseNotFound
        successResponse d = responseOk $ encode d

retrieveUserHandler :: Handler
retrieveUserHandler = do
    pks <- asks hPks
    case lookup "pk" pks >>= (readMaybe . unpack) of
        Nothing -> responseNotFound
        Just pk -> do
            conn <- asks hConnection
            res  <- liftIO $ DB.selectUser conn pk
            either errorResponse successResponse res
  where
    errorResponse :: EX.SomeException -> MonadHandler Response
    errorResponse _ = responseNotFound
    successResponse Nothing     = responseNotFound
    successResponse (Just user) = responseOk $ encode user

updateUserHandler :: Handler
updateUserHandler = responseOk "Update User Route!"
