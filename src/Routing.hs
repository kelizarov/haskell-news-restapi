{-# LANGUAGE OverloadedStrings #-}
module Routing where

import           Network.HTTP.Types
import           Network.Wai
import           Data.Text
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Internal as LBS

data Route = PathRoute Text Route | DynamicRoute Text Route | MethodRoute Method

type Handler = Request -> Response

rootRoute :: Route
rootRoute = MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "POST"

retrieveUserRoute :: Route
retrieveUserRoute =
    PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute "GET"

updateUserRoute :: Route
updateUserRoute =
    PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute "GET"

responseOk :: LBS.ByteString -> Response
responseOk = responseLBS status200 [(hContentType, "text/plain")]

responseError :: LBS.ByteString -> Response
responseError = responseLBS status400 [(hContentType, "text/plain")]

createUserHandler :: Handler
createUserHandler _ = responseOk "Create User Route!"

retrieveUserHandler :: Handler
retrieveUserHandler _ = responseOk "Retrive User Route!"

updateUserHandler :: Handler
updateUserHandler _ = responseOk "Update User Route!"    

routes :: [(Route, Handler)]
routes =
    [ (createUserRoute  , createUserHandler)
    , (retrieveUserRoute, retrieveUserHandler)
    , (updateUserRoute  , updateUserHandler)
    ]

routeExists :: Route -> [Text] -> Method -> Bool
routeExists (MethodRoute r) [] method | r == method = True
                                      | otherwise   = False
routeExists (MethodRoute r) xs _ = False
routeExists r               [] _ = False
routeExists (PathRoute r rs) (x : xs) method
    | r == x    = routeExists rs xs method
    | otherwise = False
routeExists (DynamicRoute _ rs) (_ : xs) method = routeExists rs xs method

route :: [(Route, Handler)] -> Request -> IO Response
route [] req = pure $ responseError "Not Found!"
route (x : xs) req
    | routeExists (fst x) (pathInfo req) (requestMethod req) = pure $ snd x req
    | otherwise = route xs req
