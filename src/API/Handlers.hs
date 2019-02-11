{-# LANGUAGE OverloadedStrings #-}

module API.Handlers
  ( Handler
  , createUserHandler
  , retrieveUserHandler
  , updateUserHandler
  , listUserHandler
  , responseOk
  , responseError
  ) where

import qualified Control.Exception as EX
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Core.Config as C
import qualified Core.Database as DB
import Core.Monad.Handler
import Core.Monad.Logger
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.Text
import Models.User
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import Text.Read

responseOk :: Applicative m => LBS.ByteString -> m Response
responseOk b =
  pure $ responseLBS HTTP.status200 [(HTTP.hContentType, "text/plain")] b

responseError :: Applicative m => LBS.ByteString -> m Response
responseError b =
  pure $ responseLBS HTTP.status400 [(HTTP.hContentType, "text/plain")] b

createUserHandler :: Handler
createUserHandler = do
  req <- asks hRequest
  body <- liftIO $ strictRequestBody req
  logDebug . pack . show $ body
  either errorValidation successValidation (eitherDecode body)
  where
    errorValidation _ = responseError "Error parsing JSON"
    successValidation d = do
      conn <- asks hConnection
      query <-
        liftIO $
        EX.try $
        DB.insert
          conn
          defaultUser
            { userFirstName = userRawFirstName d
            , userLastName = userRawLastName d
            , userIsAdmin = fromMaybe False (userRawIsAdmin d)
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
      res <- liftIO $ EX.try $ DB.select conn pk
      either errorResponse successResponse res
  where
    errorResponse :: EX.SomeException -> MonadHandler Response
    errorResponse err = responseError $ (LBS.pack . show) err
    successResponse :: Maybe User -> MonadHandler Response
    successResponse Nothing = responseError "Object not found"
    successResponse (Just user) = responseOk $ encode user

listUserHandler :: Handler
listUserHandler = do
  conn <- asks hConnection
  res <- liftIO $ EX.try $ DB.list conn (20, 0)
  either error success res
  where
    error :: EX.SomeException -> MonadHandler Response
    error err = responseError $ (LBS.pack . show) err
    success :: [User] -> MonadHandler Response
    success us = responseOk $ encode us

updateUserHandler :: Handler
updateUserHandler = responseOk "Update User Route!"
