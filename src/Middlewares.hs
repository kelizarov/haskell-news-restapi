{-# LANGUAGE OverloadedStrings #-}
module Middlewares where

import           Network.Wai
import           Handlers
import           Models
import           Database
import           Data.Maybe
import           Text.Read
import           MonadHandler
import           Control.Monad.Reader
import qualified Control.Exception             as EX
import qualified Data.Text                     as T
import qualified Config                        as C
import qualified Data.ByteString.Char8         as BS


data Permission = IsAdmin | OwnerOf

processHandler :: Handler -> [Permission] -> Handler
processHandler handler [] = handler
processHandler handler ps = do
    user <- getRequestUser
    case user of
        Nothing -> responseNotFound
        Just u | True      -> handler
               | otherwise -> responseNotFound

isAllowed :: User -> Permission -> Bool
isAllowed user IsAdmin = undefined
isAllowed user OwnerOf = undefined
isAllowed _    _       = False
