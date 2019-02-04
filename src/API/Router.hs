{-# LANGUAGE OverloadedStrings #-}
module API.Router where

import           Network.HTTP.Types
import           Network.Wai
import           Data.Maybe
import           Control.Monad
import           Data.Aeson
import           Text.Read
import qualified Control.Exception             as EX
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple    as PSQL

import qualified Core.Database                 as DB
import qualified Core.Config                   as C
import           Core.Monad.Handler
import           API.Middleware
import           API.Handlers

data Route = PathRoute T.Text Route | DynamicRoute T.Text Route | MethodRoute Method

rootRoute :: Route
rootRoute = MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "POST"

retrieveUserRoute :: Route
retrieveUserRoute =
    PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute "GET"

listUserRoute :: Route
listUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "GET"

updateUserRoute :: Route
updateUserRoute =
    PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute
        "PATCH"

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "POST"

retrieveAuthorRoute :: Route
retrieveAuthorRoute =
    PathRoute "api" $ PathRoute "authors" $ DynamicRoute "pk" $ MethodRoute
        "GET"

updateAuthorRoute :: Route
updateAuthorRoute =
    PathRoute "api" $ PathRoute "authors" $ DynamicRoute "pk" $ MethodRoute
        "PATCH"

traverseRoute :: Request -> Route -> (Bool, [(T.Text, T.Text)])
traverseRoute req route = checkRoute route
                                     (pathInfo req)
                                     (requestMethod req)
                                     []
  where
    checkRoute (MethodRoute r) [] method pks | r == method = (True, pks)
                                             | otherwise   = (False, pks)
    checkRoute (MethodRoute r) xs _ pks = (False, pks)
    checkRoute r               [] _ pks = (False, pks)
    checkRoute (PathRoute r rs) (x : xs) method pks
        | r == x    = checkRoute rs xs method pks
        | otherwise = (False, pks)
    checkRoute (DynamicRoute r rs) (x : xs) method pks =
        checkRoute rs xs method ((r, x) : pks)

routeTable :: [(Route, Handler)]
routeTable =
    [ (createUserRoute  , createUserHandler)
    , (retrieveUserRoute, withPermission retrieveUserHandler [Admin])
    , (updateUserRoute  , updateUserHandler)
    , (listUserRoute    , withPermission listUserHandler [Admin])
    ]

route :: C.Config -> [(Route, Handler)] -> Request -> IO Response
route _    []       _   = responseError "Resource not found"
route conf (x : xs) req = case r of
    (True, pk) -> EX.bracket openConnection PSQL.close
        $ \conn -> runHandler conf pk req conn handler
    (False, _) -> route conf xs req
  where
    r              = traverseRoute req (fst x)
    openConnection = DB.connect conf
    handler        = snd x

