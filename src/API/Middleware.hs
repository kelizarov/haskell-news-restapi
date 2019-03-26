{-# LANGUAGE OverloadedStrings #-}

module API.Middleware where

import qualified Control.Exception as EX
import Control.Monad.Except
import Control.Monad.Reader
import qualified Core.Config as C
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Text as T
import Models.User
import Network.Wai
import Text.Read

import API.Handlers
import Core.Exceptions
import Core.Monad.Handler

data Permission
  = Admin
  | Owner
  | Author
  | Authorized

withPermission :: Permission -> MonadHandler Bool
withPermission Admin = do
  user <- getRequestUser
  case user of
    Nothing -> throwError Unauthorized
    Just u -> return $ uIsAdmin u
withPermission Authorized = do
  user <- getRequestUser
  case user of
    Nothing -> throwError Unauthorized
    Just _ -> return True
withPermission _ = return True

getRequestUser :: MonadHandler (Maybe User)
getRequestUser = do
  conn <- asks hConnection
  req <- asks hRequest
  pks <- asks hPks
  case lookup "Authorization" (requestHeaders req) >>= (readMaybe . BS.unpack) of
    Nothing -> pure Nothing
    Just uId -> queryUser (UserById uId)
