{-# LANGUAGE OverloadedStrings #-}
module API.Handlers
    ( Handler
    , createUserHandler
    , retrieveUserHandler
    , updateUserHandler
    , responseOk
    , responseError
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

import           Models.User
import           Core.Monad.Handler
import qualified Core.Database                 as DB
import qualified Core.Config                   as C

responseOk :: Applicative m => LBS.ByteString -> m Response
responseOk b =
    pure $ responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] b

responseError :: Applicative m => LBS.ByteString -> m Response
responseError b = pure $ responseLBS HTTP.status400
                                      [(HTTP.hContentType, "text/plain")]
                                      b

createUserHandler :: Handler
createUserHandler = do
    req  <- asks hRequest
    body <- liftIO $ strictRequestBody req
    either errorValidation successValidation (eitherDecode body)
  where
    errorValidation _ = responseError "Error parsing JSON"
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
        errorResponse err = responseError $ (LBS.pack . show) err
        successResponse d = responseOk $ encode d

retrieveUserHandler :: Handler
retrieveUserHandler = do
    pks <- asks hPks
    case lookup "pk" pks >>= (readMaybe . unpack) of
        Nothing -> responseError "Wrong pk"
        Just pk -> do
            conn <- asks hConnection
            res  <- liftIO $ DB.selectUser conn pk
            either errorResponse successResponse res
  where
    errorResponse :: EX.SomeException -> MonadHandler Response
    errorResponse err = responseError $ (LBS.pack . show) err
    successResponse Nothing     = responseError "Object not found"
    successResponse (Just user) = responseOk $ encode user

listUserHandler :: Handler
listUserHandler = undefined

updateUserHandler :: Handler
updateUserHandler = responseOk "Update User Route!"
