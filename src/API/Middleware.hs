{-# LANGUAGE OverloadedStrings #-}

module API.Middleware where

import           Network.Wai
import           Data.Maybe
import           Text.Read
import           Control.Monad.Reader
import qualified Control.Exception             as EX
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as BS

import           API.Handlers
import           Models.User
import           Core.Monad.Handler
import qualified Core.Config                   as C

data Permission = Admin | Owner | Author

withPermission :: Handler -> [Permission] -> Handler
withPermission handler [] = handler
withPermission handler ps = do
    user <- getRequestUser
    case user of
        Nothing -> responseError "Permission denied"
        Just u | all (checkPermission u) ps -> handler
               | otherwise                  -> responseError "Permission denied"
  where
    checkPermission user Admin = userIsAdmin user
    checkPermission user Owner = True
    checkPermission user _     = True

