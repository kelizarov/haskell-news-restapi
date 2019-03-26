{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module API.Handlers
  ( createUserHandler
  , retrieveUserHandler
  , updateUserHandler
  , listUserHandler
  , responseOk
  , responseError
  ) where

import API.Responses
import qualified Control.Exception as EX
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Core.Config as C
import Core.Exceptions
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

createUserHandler :: MonadHandler Response
createUserHandler = do
  req <- asks hRequest
  body <- liftIO $ strictRequestBody req
  logDebug . pack . show $ body
  either errorValidation successValidation (eitherDecode body)
  where
    errorValidation err = throwError $ ParseError "Error parsing JSON"
    successValidation d = do
      user <- createUser d
      (responseOk . encode) user

retrieveUserHandler :: MonadHandler Response
retrieveUserHandler = do
  pks <- asks hPks
  case lookup "pk" pks >>= (readMaybe . unpack) of
    Nothing -> throwError $ SQLError "Wrong pk"
    Just pk -> do
      conn <- asks hConnection
      res <- queryUser $ UserById pk
      case res of
        Nothing -> throwError $ SQLError "User not found"
        Just user -> (responseOk . encode) user

listUserHandler :: MonadHandler Response
listUserHandler = do
  pks <- asks hPks
  queryUser (UserList (0, 20)) >>= responseOk . encode

updateUserHandler :: MonadHandler Response
updateUserHandler = do
  pks <- asks hPks
  case lookup "pk" pks >>= (readMaybe . unpack) of
    Nothing -> throwError $ SQLError "Wrong pk"
    Just pk -> do
      req <- asks hRequest
      body <- liftIO $ strictRequestBody req
      either errorValidation (successValidation pk) (eitherDecode body)
  where
    errorValidation err = throwError $ ParseError "Error parsing JSON"
    successValidation pk d = updateUser pk d >>= responseOk . encode
