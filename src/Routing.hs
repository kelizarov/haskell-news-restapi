{-# LANGUAGE OverloadedStrings #-}
module Routing where

import           Network.HTTP.Types
import           Network.Wai
import           Database
import           Models
import           Serializers
import           Data.Text
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Internal as LBS

data Route = PathRoute Text Route | DynamicRoute Text Route | MethodRoute Method

type Handler = Request -> Text -> IO Response

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

responseOk :: LBS.ByteString -> Response
responseOk = responseLBS status200 [(hContentType, "text/plain")]

responseError :: LBS.ByteString -> Response
responseError = responseLBS status400 [(hContentType, "text/plain")]

createUserHandler :: Handler
createUserHandler req _ = do
    body <- strictRequestBody req
    case eitherDecode body of
        Left  s -> return $ responseError $ encode s
        Right d -> do
            user <- insertUser userDefault
                { userFirstName = createUserRawFirstName d
                , userLastName  = createUserRawLastName d
                }
            return $ responseOk $ encode (serializeUser user)


retrieveUserHandler :: Handler
retrieveUserHandler _ pk = do
    putStrLn $ unpack pk
    res <- selectUser $ read (unpack pk)
    case res of
        Nothing   -> pure $ responseError "No user found!"
        Just user -> pure $ responseOk $ encode (serializeUser user)

updateUserHandler :: Handler
updateUserHandler _ _ = pure $ responseOk "Update User Route!"

routes :: [(Route, Handler)]
routes =
    [ (createUserRoute  , createUserHandler)
    , (retrieveUserRoute, retrieveUserHandler)
    , (updateUserRoute  , updateUserHandler)
    ]

getPk :: Route -> [Text] -> Text
getPk (DynamicRoute _ _ ) (x:xs) = x
getPk (PathRoute    _ rs) (_:xs) = getPk rs xs
getPk _                _   = ""

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
    | routeExists (fst x) (pathInfo req) (requestMethod req) = snd
        x
        req
        (getPk (fst x) (pathInfo req))
    | otherwise = route xs req
