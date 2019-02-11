{-# LANGUAGE OverloadedStrings #-}

module API.Middleware where

import API.Handlers
import qualified Control.Exception as EX
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
  | Owner
  | Author

withPermission :: Handler -> [Permission] -> Handler
withPermission handler [] = handler
withPermission handler ps = do
  user <- getRequestUser
  case user of
    Nothing -> responseError "Permission denied"
    Just u
      | all (checkPermission u) ps -> handler
      | otherwise -> responseError "Permission denied"
  where
    checkPermission user Admin = userIsAdmin user
    checkPermission user Owner = True
    checkPermission user _ = True
