{-# LANGUAGE OverloadedStrings #-}

module News.Services.Server where

import qualified Data.ByteString               as BS
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as HTTP

data API
  = POST [T.Text]
  | GET [T.Text]
  | PATCH [T.Text]
  | DELETE [T.Text]
  | UNKNOWN
  deriving (Show, Eq)

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
matches :: [T.Text] -> [T.Text] -> Maybe [Int]
matches path reqPath = checkRoute (T.unpack <$> path) (T.unpack <$> reqPath) []
 where
  checkRoute (x : xs) (y : ys) pks
    | x == y     = checkRoute xs ys pks
    | x == ":pk" = checkRoute xs ys (read y : pks)
    | otherwise  = Nothing
  checkRoute x y pks | x == y    = Just pks
                     | otherwise = Nothing
