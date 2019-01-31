{-# LANGUAGE OverloadedStrings #-}
module Routing where

import           Network.HTTP.Types
import           Network.Wai
import           Models
import           Middlewares
import           Handlers
import           MonadHandler
import           Database
import           Data.Maybe
import           Control.Monad
import qualified          Control.Exception as EX
import           Data.Aeson
import           Text.Read
import qualified Data.Text                     as T
import qualified Config                        as C
import qualified Database.PostgreSQL.Simple    as PSQL

data Route = PathRoute T.Text Route | DynamicRoute T.Text Route | MethodRoute Method

rootRoute :: Route
rootRoute = MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "POST"

retrieveUserRoute :: Route
retrieveUserRoute =
    PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute "GET"

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

getPk :: Request -> Route -> Maybe Int
getPk req route = parsePathInfo route $ map T.unpack $ pathInfo req
  where
    parsePathInfo (MethodRoute _    ) _        = Nothing
    parsePathInfo (PathRoute    h hs) (x : xs) = parsePathInfo hs xs
    parsePathInfo (DynamicRoute _ _ ) (x : _ ) = readMaybe x

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

routes :: [(Route, Handler)]
routes =
    [ (createUserRoute  , createUserHandler)
    , (retrieveUserRoute, retrieveUserHandler)
    , (updateUserRoute  , updateUserHandler)
    ]

route :: C.Config -> [(Route, Handler)] -> Request -> IO Response
route _ [] _ = responseNotFound
route conf (x : xs) req =
    let r = traverseRoute req (fst x)
    in  case r of
            (True, pk) -> 
                EX.bracket openConnection PSQL.close
                    $ \conn -> runHandler conf pk req conn handler
            (False, _) -> route conf xs req
    where
        handler = snd x
        openConnection = connectInfo conf >>= PSQL.connect

