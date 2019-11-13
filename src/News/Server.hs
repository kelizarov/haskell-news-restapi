{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module News.Server
  ( route
  )
where

import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Network.HTTP.Types             ( statusCode )
import qualified Network.HTTP.Types            as HTTP
import           Network.Wai                    ( Middleware
                                                , rawPathInfo
                                                , rawQueryString
                                                , remoteHost
                                                , requestMethod
                                                , responseStatus
                                                )
import qualified Network.Wai                   as HTTP
import           Network.Wai.Handler.Warp       ( run )

import           News.Config
import           News.AppHandle
import           News.Endpoints.User
import           News.Env

type Path = [T.Text]

data API
  = POST Path
  | GET Path
  | PATCH Path
  | DELETE Path
  | UNKNOWN
  deriving (Show, Eq)

route :: HTTP.Request -> Application HTTP.Response
route req = case methodAndPath req of
  POST (matches ["api", "user"] -> Just []) -> do
    res <- createUserHandler
    pure $ HTTP.responseLBS HTTP.status200 [] "Success"
  GET (matches ["api", "user", ":pk"] -> Just [userId]) ->
    pure $ HTTP.responseLBS HTTP.status501 [] ""
  PATCH (matches ["api", "user", ":pk"] -> Just [userId]) ->
    pure $ HTTP.responseLBS HTTP.status501 [] ""
  UNKNOWN -> pure $ HTTP.responseLBS HTTP.status404 [] ""

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
