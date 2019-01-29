{-# LANGUAGE OverloadedStrings #-}
module Routing where

import           Network.HTTP.Types
import           Network.Wai
import           Database
import           Models
import           Serializers
import           Data.Maybe
import           Control.Monad
import           Control.Exception              ( catch
                                                , SomeException
                                                )
import qualified          Data.Text as T
import           Data.Aeson
import           Text.Read
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as LBS

data Route = PathRoute T.Text Route | DynamicRoute T.Text Route | MethodRoute Method

type Handler = Request -> IO Response

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

responseNotFound :: Response
responseNotFound =
    responseLBS status404 [(hContentType, "text/plain")] "Resource not found"

createUserHandler :: Handler
createUserHandler req = do
    body <- strictRequestBody req
    either errorValidation successValidation (eitherDecode body)
  where
    errorValidation _ = pure responseNotFound
    successValidation d = do
        query <- insertUser defaultUser
            { userFirstName = createUserRawFirstName d
            , userLastName  = createUserRawLastName d
            , userIsAdmin   = fromMaybe False (createUserRawIsAdmin d)
            }
        either errorResponse successResponse query
      where
        errorResponse :: SomeException -> IO Response
        errorResponse _ = pure responseNotFound
        successResponse d = pure $ responseOk $ encode (serializeUserSuccess d)

retrieveUserHandler :: Handler
retrieveUserHandler _ = do
    res <- selectUser 0
    either errorResponse successResponse res
  where
    errorResponse :: SomeException -> IO Response
    errorResponse _ = pure responseNotFound
    successResponse Nothing = pure responseNotFound
    successResponse (Just user) =
        pure $ responseOk $ encode (serializeUserSuccess user)

updateUserHandler :: Handler
updateUserHandler _ = pure $ responseOk "Update User Route!"

routes :: [(Route, Handler)]
routes =
    [ (createUserRoute  , createUserHandler)
    , (retrieveUserRoute, retrieveUserHandler)
    , (updateUserRoute  , updateUserHandler)
    ]

getPk :: Request -> Route -> Maybe Int
getPk req route = parsePathInfo route $ map T.unpack $ pathInfo req
  where
    parsePathInfo (MethodRoute _    ) _       = Nothing
    parsePathInfo (PathRoute    h hs) (x : xs) = parsePathInfo hs xs
    parsePathInfo (DynamicRoute _ _ ) (x : _ ) = readMaybe x

routeExists :: Request -> Route -> Bool
routeExists req route = checkRoute route (pathInfo req) (requestMethod req)
  where
    checkRoute (MethodRoute r) [] method | r == method = True
                                         | otherwise   = False
    checkRoute (MethodRoute r) xs _ = False
    checkRoute r               [] _ = False
    checkRoute (PathRoute r rs) (x : xs) method
        | r == x    = checkRoute rs xs method
        | otherwise = False
    checkRoute (DynamicRoute _ rs) (_ : xs) method = checkRoute rs xs method

route :: [(Route, Handler)] -> Request -> IO Response
route [] req = pure responseNotFound
route (x : xs) req | routeExists req (fst x) = snd x req
                   | otherwise               = route xs req
