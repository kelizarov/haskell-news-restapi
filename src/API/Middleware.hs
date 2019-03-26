{-# LANGUAGE OverloadedStrings #-}

module API.Middleware where

import API.Handlers
import qualified Control.Exception as EX
import Control.Monad.Except
import Control.Monad.Reader
import qualified Core.Config as C
import Core.Monad.Handler
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Text as T
import Models.User
import Network.Wai
import Text.Read

data Permission
  = Admin
  | Authorized
  | Owner
  | Author

withPermission :: [Permission] -> MonadHandler Bool
withPermission _ = undefined
    
getRequestUser :: MonadHandler (Maybe User)
getRequestUser = do
  conn <- asks hConnection
  req <- asks hRequest
  pks <- asks hPks
  case lookup "Authorization" (requestHeaders req) >>= (readMaybe . BS.unpack) of
    Nothing -> pure Nothing
    Just uId -> queryUser (UserById uId)
