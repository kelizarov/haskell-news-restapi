{-# LANGUAGE OverloadedStrings #-}

module API.Router where

import qualified Control.Exception as EX
import Control.Monad
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types
import Network.Wai
import Text.Read

import API.Handlers
import API.Middleware
import API.Responses
import qualified Core.Config as C
import qualified Core.Database as DB
import Core.Exceptions
import Core.Monad.Handler

type Path = [T.Text]

type Route = (Path, Method, MonadHandler Response, Permission)

type Routes = [Route]

type PK = (T.Text, T.Text)

traverseRoute :: Request -> Path -> Method -> (Bool, [PK])
traverseRoute req route method
  | method /= requestMethod req = (False, [])
  | otherwise = checkRoute (T.unpack <$> route) (T.unpack <$> pathInfo req) []
  where
    checkRoute (x:xs) (y:ys) pks
      | x == y = checkRoute xs ys pks
      | L.head x == ':' = checkRoute xs ys ((T.pack $ L.tail x, T.pack y) : pks)
      | otherwise = (False, [])
    checkRoute x y pks
      | x == y = (True, pks)
      | otherwise = (False, [])

routeTable :: Routes
routeTable =
  [ (["api", "users"], methodPost, createUserHandler, Admin)
  , (["api", "users", ":pk"], methodGet, retrieveUserHandler, Authorized)
  , (["api", "users", ":pk"], methodPatch, updateUserHandler, Authorized)
  , (["api", "users"], methodGet, listUserHandler, Authorized)
  ]

route :: C.Config -> Routes -> Request -> IO Response
route _ [] _ = responseError "Resource not found"
route conf (x:xs) req =
  case r of
    (True, pk) ->
      EX.bracket openConnection PSQL.close $ \conn -> do
        res <- runHandler conf pk req conn $ do
          isAllowed <- withPermission permission
          if isAllowed
            then handler
            else throwError Forbidden
        either left pure res
    (False, _) -> route conf xs req
  where
    r = traverseRoute req path method
    openConnection = DB.connect conf
    (path, method, handler, permission) = x
    left (ParseError err) =
      (responseError . LBS.pack) $ "Error parsing data: " <> err
    left (SQLError err) =
      (responseError . LBS.pack) $ "SQL error occured: " <> err
    left Forbidden = (responseError . LBS.pack) "Access denied"
    left Unauthorized = (responseError . LBS.pack) "Unauthorized"
