{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.Except
import API.Responses
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Core.Config as C
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
      conn <- asks hConnection
      user <- createUser d
      (responseOk . encode) user

retrieveUserHandler :: MonadHandler Response
retrieveUserHandler = do
  pks <- asks hPks
  case lookup "pk" pks >>= (readMaybe . unpack) of
    Nothing -> throwError $ SQLError "Wrong pk"
    Just pk -> do
      conn <- asks hConnection
      res <- getUser pk
      case res of
        Nothing -> throwError $ SQLError "User not found"
        Just user -> (responseOk . encode) user

listUserHandler :: MonadHandler Response
listUserHandler = do
  conn <- asks hConnection
  res <- listUser (0, 0)
  (responseOk . encode) res

updateUserHandler :: MonadHandler Response
updateUserHandler = responseOk "Update User Route!"
