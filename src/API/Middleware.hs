module API.Middleware where

import           Network.Wai
import           Data.Maybe
import           Text.Read
import           Control.Monad.Reader
import qualified Control.Exception             as EX
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as BS

import           API.Handlers
import Models.User
import           Core.Monad.Handler
import qualified Core.Config                   as C

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
