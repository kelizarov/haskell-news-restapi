{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module News.Server
  ( api
  )
where

import qualified Data.ByteString.Char8         as BS
import           Network.HTTP.Types             ( statusCode )
import           Network.Wai                    ( Middleware
                                                , remoteHost
                                                , requestMethod
                                                , rawPathInfo
                                                , rawQueryString
                                                , responseStatus
                                                )
import           Network.Wai.Handler.Warp       ( run )
-- import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as HTTP

import           News.Endpoints.User
import           News.Config
import           News.Env

type Path = [T.Text]

data API
  = POST Path
  | GET Path
  | PATCH Path
  | DELETE Path
  | UNKNOWN
  deriving (Show, Eq)

api :: HTTP.Request -> IO HTTP.Response
api req = case methodAndPath req of
  POST (matches ["api", "user"] -> Just []) -> do
    conf <- loadConfigFile Dev
    res  <- createUserHandler conf req
    pure $ HTTP.responseLBS HTTP.status200 [] "Success"
  GET (matches ["api", "user", ":pk"] -> Just [userId]) -> undefined
  PATCH (matches ["api", "user", ":pk"] -> Just [userId]) -> undefined
  UNKNOWN -> undefined

methodAndPath :: HTTP.Request -> API
methodAndPath req = case getMethod of
  HTTP.POST   -> POST $ HTTP.pathInfo req
  HTTP.GET    -> GET $ HTTP.pathInfo req
  HTTP.PATCH  -> PATCH $ HTTP.pathInfo req
  HTTP.DELETE -> DELETE $ HTTP.pathInfo req
  _           -> UNKNOWN
 where
  getMethod =
    either (error . show) id $ HTTP.parseMethod (HTTP.requestMethod req)

-- TODO refactor with fold
matches :: Path -> Path -> Maybe [Int]
matches path reqPath = checkRoute (T.unpack <$> path) (T.unpack <$> reqPath) []
 where
  checkRoute (x : xs) (y : ys) pks
    | x == y     = checkRoute xs ys pks
    | x == ":pk" = checkRoute xs ys (read y : pks)
    | otherwise  = Nothing
  checkRoute x y pks | x == y    = Just pks
                     | otherwise = Nothing
